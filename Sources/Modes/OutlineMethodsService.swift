//
//  OutlineMethodsService_refactored.swift
//  podlodkacli
//
//  Полный файл после рефакторинга с подробными комментариями на русском языке.
//  Цель: вынесение (outlining) частей методов во вспомогательные приватные функции
//  без изменения семантики исходного кода. Добавлены разъясняющие комментарии
//  о каждом шаге алгоритма.
//

import Foundation
import SwiftSyntax
import SwiftParser
import SwiftSyntaxBuilder
import SourceKittenFramework

// MARK: - InfoCollectorVisitor

/// Визитор, который проходит по AST и собирает информацию, необходимую для безопасного
/// вынесения частей методов:
/// 1) Структура кода: `ClassDeclSyntax` -> список его **прямых** методов (`FunctionDeclSyntax`).
/// 2) Для каждой функции — список локальных идентификаторов (переменных), объявленных внутри неё.
///    Это нужно, чтобы корректно построить сигнатуру вспомогательных функций и не испортить логику
///    (например, не потерять изменяемые переменные).
///
/// Ограничение: рассматриваем только методы, являющиеся прямыми детьми класса. Если функция
/// вложена в другую функцию/структуру/actor/enum — она для нас «опасная» и пропускается.
final class InfoCollectorVisitor: SyntaxVisitor {

    /// Собранная структура кода: класс -> список его методов.
    var codeStructure: [ClassDeclSyntax: [FunctionDeclSyntax]] = [:]

    /// Для каждой функции — список токенов локальных идентификаторов, объявленных в её теле.
    var localFunctionIdentificators: [FunctionDeclSyntax: [TokenSyntax]] = [:]

    private var currentFunc: FunctionDeclSyntax? = nil

    func clear() {
        codeStructure = [:]
        localFunctionIdentificators = [:]
        currentFunc = nil
    }

    // Сбор локальных идентификаторов (переменных) внутри текущей функции.
    override func visit(_ node: IdentifierPatternSyntax) -> SyntaxVisitorContinueKind {
        switch node.identifier.tokenKind {
        case .identifier(_):
            if let f = currentFunc {
                localFunctionIdentificators[f, default: []].append(node.identifier)
            }
        default:
            break
        }
        return .visitChildren
    }

    // Сбор функций и привязка их к ближайшему классу.
    override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
        guard let classNode = findParentClass(for: node),
              InfoCollectorVisitor.haveOnlyClassParent(for: node) else {
            currentFunc = nil
            return .visitChildren
        }

        currentFunc = node
        codeStructure[classNode, default: []].append(node)
        return .visitChildren
    }

    // Поиск ближайшего родителя-класса.
    private func findParentClass(for node: FunctionDeclSyntax) -> ClassDeclSyntax? {
        var cursor = node.parent
        while let parent = cursor {
            if let classDecl = parent.as(ClassDeclSyntax.self) { return classDecl }
            cursor = parent.parent
        }
        return nil
    }

    /// Проверка, что у функции в предках нет «запрещённых» узлов — только класс.
    static func haveOnlyClassParent(for node: FunctionDeclSyntax) -> Bool {
        var cursor = node.parent
        while let parent = cursor {
            if parent.as(StructDeclSyntax.self) != nil
                || parent.as(EnumDeclSyntax.self) != nil
                || parent.as(FunctionDeclSyntax.self) != nil
                || parent.as(ActorDeclSyntax.self) != nil {
                return false
            }
            cursor = parent.parent
        }
        return true
    }
}

// MARK: - OutlineMethodsService

/// Реализует сам рефакторинг: «выносит» подходящие инструкции метода во вспомогательные
/// приватные функции. Делает это консервативно, чтобы сохранить поведение:
/// - Пропускает методы с «опасными» атрибутами (`@inline`, `@objc`, и т.д.), без тела, перегрузки
///   операторов (`==` и т.п.), а также асинхронные методы (в данной версии).
/// - Опирается на список локальных идентификаторов и проверку типов через SourceKit (по возможности),
///   чтобы корректно сформировать параметры вспомогательной функции (`inout`, `@escaping`).
/// - Добавляет `@inline(never)` к новой функции, чтобы оптимизатор не склеил её обратно.
/// - В исходном методе вставляет вызов новой функции; если метод должен вернуть значение и выносимая
///   инструкция последняя — оборачивает вызов в `return`.
final class OutlineMethodsService: SyntaxRewriter {

    // Входные данные и состояние
    let infoCollector: InfoCollectorVisitor
    let sourceFilePath: String
    let splitPattern: String

    /// Счётчик — сколько частей уже вынесено для данной функции (для суффикса имени).
    var functionSplitCounter: [FunctionDeclSyntax: Int] = [:]

    init(infoCollector: InfoCollectorVisitor,
         sourceFilePath: String,
         splitPattern: String) {
        self.infoCollector = infoCollector
        self.sourceFilePath = sourceFilePath
        self.splitPattern = splitPattern
    }

    /// Основная точка входа — на уровне класса. Мы заменяем тела подходящих методов
    /// и добавляем созданные вспомогательные функции как новых членов класса.
    override func visit(_ node: ClassDeclSyntax) -> DeclSyntax {
        var members = node.members.members

        // Получаем кандидатов-методов из предварительного прохода InfoCollectorVisitor.
        guard let methods = infoCollector.codeStructure[node] else {
            return super.visit(node)
        }

        // Перебираем каждый метод класса и пытаемся вынести из него части.
        methodLoop: for method in methods {
            // 1) Пропуск по атрибутам, которые трогать нельзя (или нежелательно).
            if let attrList = method.attributes {
                for attr in attrList {
                    if let attrSyntax = attr.as(AttributeSyntax.self) {
                        let name = attrSyntax.attributeName.text
                        switch name {
                        case "inline", "objc", "inlinable", "usableFromInline":
                            continue methodLoop
                        default:
                            break
                        }
                    }
                }
            }

            // 2) Метод должен быть прямым потомком класса (без вложенности в другие конструкции).
            guard InfoCollectorVisitor.haveOnlyClassParent(for: method) else { continue }

            // 3) Наличие тела с инструкциями — обязательное условие.
            guard let statements = method.body?.statements else { continue }

            // 4) Перегрузки операторов (у нас эвристика по «=» в идентификаторе) пропускаем.
            if method.identifier.text.contains("=") { continue }

            // 5) В этой версии игнорируем async/reasync, чтобы не усложнять логику.
            if method.signature.asyncOrReasyncKeyword != nil { continue }

            // --- Состояние анализа для конкретного метода ---
            var replacedItems: [CodeBlockItemSyntax] = []                  // Новые элементы тела
            var declTypeByToken: [TokenSyntax: TypeSyntaxProtocol?] = [:]  // Типы локальных переменных
            var letDeclTypeByToken: [TokenSyntax: TypeSyntaxProtocol?] = [:] // Подмножество для let
            var failedTypeCheckTokens: [TokenSyntax] = []                  // Идентификаторы, чьи типы не удалось определить
            var methodParams: [ParameterNames: TypeSyntaxProtocol?] = [:]  // Параметры исходного метода
            var haveParameterTrouble = false                               // Флаг «плохих» параметров

            // 6) Анализ параметров метода. Отсеиваем случаи, где корректная сигнатура помощника
            //    потенциально невозможна или рискованна.
            for param in method.signature.input.parameterList {
                // Пропускаем pack-расширения и подобные конструкции.
                if param.type?.as(PackExpansionTypeSyntax.self) != nil { haveParameterTrouble = true; break }
                // Пропускаем `Any` как ненадёжный тип для наших целей.
                if let simple = param.type?.as(SimpleTypeIdentifierSyntax.self), simple.name.text == "Any" {
                    haveParameterTrouble = true; break
                }

                let pair = ParameterNames(firstParameter: param.firstName, secondParameter: param.secondName)

                if let second = param.secondName { // есть external и internal имя
                    if second.text == "_" { haveParameterTrouble = true; break }
                    methodParams[pair] = param.type
                } else if let first = param.firstName, first.text != "_" { // одно имя
                    methodParams[pair] = param.type
                } else {
                    haveParameterTrouble = true; break
                }
            }
            if haveParameterTrouble { continue }

            // 7) Проходим по инструкциям тела и решаем: вынести или оставить на месте.
            instructionLoop: for (index, item) in statements.enumerated() {
                // 7.1 Если это объявление переменной — собираем типы, чтобы потом правильно
                //     сформировать список параметров вспомогательной функции.
                if let varDecl = item.item.as(VariableDeclSyntax.self) {
                    // Эту инструкцию сохраняем как есть (в новом теле исходного метода)
                    let newItem = item.withItem(.init(varDecl))

                    let isLet = varDecl.letOrVarKeyword.text == "let"
                    for bind in varDecl.bindings {
                        // Случай с кортежем в паттерне пропускаем как «опасный» для нас.
                        if bind.pattern.as(TuplePatternSyntax.self) != nil { continue methodLoop }
                        guard let id = bind.pattern.as(IdentifierPatternSyntax.self) else { continue }
                        // Если нет инициализатора — тип не вывести (как правило), помечаем как «не удалось».
                        guard bind.initializer != nil else { failedTypeCheckTokens.append(id.identifier); continue }

                        if let annotated = bind.typeAnnotation?.type {
                            // Тип напрямую указан в коде — используем его.
                            declTypeByToken[id.identifier] = annotated
                            if isLet { letDeclTypeByToken[id.identifier] = annotated }
                        } else if let inferred = TypeCheckService.getType(pattern: id, filePath: sourceFilePath) {
                            // Пытаемся спросить тип у SourceKit, если аннотации нет.
                            declTypeByToken[id.identifier] = inferred
                            if isLet { letDeclTypeByToken[id.identifier] = inferred }
                        } else {
                            // Не смогли определить тип — добавляем в «провалившиеся».
                            failedTypeCheckTokens.append(id.identifier)
                        }

                        // Если тип явно «плохой» (например, Set или Any) — пропускаем весь метод.
                        if let t = declTypeByToken[id.identifier] ?? nil {
                            let bad = BadTypeVisitor(viewMode: .all); bad.walk(t)
                            if bad.skip { continue methodLoop }
                        }
                    }

                    // Сохраняем объявление с исходными trivia/отступами.
                    if let lead = item.leadingTrivia, let trail = item.trailingTrivia {
                        replacedItems.append(newItem.withLeadingTrivia(lead).withTrailingTrivia(trail))
                    } else {
                        replacedItems.append(newItem.withLeadingTrivia([.newlines(1), .spaces(4)]))
                    }
                    continue
                }

                // 7.2 Обрабатываем guard с биндингами (let/var), собирая типы новых переменных.
                if let guardStmt = item.item.as(GuardStmtSyntax.self) {
                    var newConds: [ConditionElementSyntax] = []

                    for cond in guardStmt.conditions {
                        // Пример: `guard let x: T = ...` — извлекаем тип x
                        if let bind = cond.condition.as(OptionalBindingConditionSyntax.self),
                           let pat = bind.pattern.as(IdentifierPatternSyntax.self) {
                            let isLet = bind.letOrVarKeyword.text == "let"
                            if let annotated = bind.typeAnnotation?.type {
                                declTypeByToken[pat.identifier] = annotated
                                if isLet { letDeclTypeByToken[pat.identifier] = annotated }
                            } else if let inferred = TypeCheckService.getType(pattern: pat, filePath: sourceFilePath) {
                                declTypeByToken[pat.identifier] = inferred
                                if isLet { letDeclTypeByToken[pat.identifier] = inferred }
                            } else {
                                failedTypeCheckTokens.append(pat.identifier)
                            }
                            newConds.append(cond)
                        } else {
                            newConds.append(cond)
                        }

                        // Случай сопоставления с образцом, где внутри вызывается функция и происходит привязка:
                        // `case let x(...)` — пытаемся вытащить идентификатор и определить тип.
                        if let matching = cond.condition.as(MatchingPatternConditionSyntax.self),
                           let exprPat = matching.pattern.as(ExpressionPatternSyntax.self),
                           let call = exprPat.expression.as(FunctionCallExprSyntax.self) {
                            for arg in call.argumentList {
                                if let unresolved = arg.expression.as(UnresolvedPatternExprSyntax.self),
                                   let binding = unresolved.pattern.as(ValueBindingPatternSyntax.self),
                                   let idPat = binding.valuePattern.as(IdentifierPatternSyntax.self) {
                                    let isLet = binding.letOrVarKeyword.text == "let"
                                    if let inferred = TypeCheckService.getType(pattern: idPat, filePath: sourceFilePath) {
                                        declTypeByToken[idPat.identifier] = inferred
                                        if isLet { letDeclTypeByToken[idPat.identifier] = inferred }
                                    } else {
                                        failedTypeCheckTokens.append(idPat.identifier)
                                    }
                                }
                            }
                        }
                    }

                    let newGuard = guardStmt.withConditions(ConditionElementListSyntax(newConds))
                    replacedItems.append(item.withItem(.init(newGuard)))
                    continue
                }

                // 7.3 Если внутри инструкции есть замыкания, которые захватывают изменяемые (var)
                //     локальные переменные — не выносим такую инструкцию (иначе изменим семантику).
                let closureV = ClosureVisitor(viewMode: .all); closureV.walk(item)
                for closure in closureV.closureList {
                    let varV = VariableVisitor(viewMode: .all)
                    // Составим список «изменяемых» локальных (все, кто не в let-списке).
                    var mutable: [TokenSyntax] = []
                    for (tok, _) in declTypeByToken where !letDeclTypeByToken.keys.contains(where: { $0.text == tok.text }) {
                        mutable.append(tok)
                    }
                    varV.variableList = mutable
                    varV.walk(closure)
                    if varV.haveIntersect {
                        // Инструкция использует изменяемое состояние в замыкании — оставляем inline.
                        replacedItems.append(item)
                        continue instructionLoop
                    }
                }

                // 7.4 Фильтрация по «опасным» вложенным конструкциям: вложенные функции, типы, defer и т.д.
                let badly = BadlyInsideVisitor(viewMode: .all); badly.walk(item)
                if badly.hasBadlyInside {
                    replacedItems.append(item)
                    continue methodLoop
                }
                // Если внутри есть return, и это не последняя инструкция, тоже оставляем inline.
                if badly.hasReturnInside && index + 1 < statements.count {
                    replacedItems.append(item)
                    continue
                }

                // 7.5 Если есть локальные переменные, чьи типы мы так и не смогли вывести — оставляем inline.
                if let localVars = infoCollector.localFunctionIdentificators[method] {
                    let varsSet = Set(localVars)
                    let failedSet = Set(failedTypeCheckTokens)
                    if !varsSet.intersection(failedSet).isEmpty {
                        replacedItems.append(item)
                        continue
                    }
                }

                // 7.6 Добрались до безопасного случая — выносим эту инструкцию в отдельный метод.
                let partIndex = (functionSplitCounter[method] ?? -1) + 1
                functionSplitCounter[method] = partIndex

                // Имя новой функции: _<исходноеИмя><splitPattern><номер>
                let partName = "_\(method.identifier.text)\(splitPattern)\(partIndex)"
                let newFuncIdent = TokenSyntax(.identifier(partName), leadingTrivia: .spaces(1), presence: .present)

                // Формируем список параметров вспомогательной функции:
                // объединяем параметры исходного метода и локальные переменные, используемые в теле.
                var mergedParams: [ParameterNames: TypeSyntaxProtocol?] = methodParams
                for (tok, t) in declTypeByToken {
                    var matchedExisting = false
                    for k in methodParams.keys where k.isEqual(to: tok) { mergedParams[k] = t; matchedExisting = true }
                    if !matchedExisting { mergedParams[ParameterNames(firstParameter: tok)] = t }
                }

                // Подготовим атрибут @escaping, который будем добавлять к типам-замыканиям.
                let escapingAttr: AttributeListSyntax.Element = {
                    AttributeSyntax(
                        atSignToken: .atSign.withLeadingTrivia([.spaces(1)]),
                        attributeName: .identifier("escaping")
                    ).as(AttributeListSyntax.Element.self)!
                }()

                // Строим список параметров helper-функции, учитывая нужду в `inout` и `@escaping`.
                var helperParamList: [FunctionParameterSyntax] = []
                helperParamList.reserveCapacity(mergedParams.count)

                var ordinal = 1
                for (paramNames, typeOpt) in mergedParams {
                    guard let paramType = typeOpt else { continue }

                    // Был ли параметр объявлен в теле (а не в сигнатуре метода)?
                    let declaredInBody = declTypeByToken.keys.contains(where: { paramNames.isEqual(to: $0) })

                    // Нужен ли `inout`? — только для изменяемых локальных переменных (не let и не параметры метода).
                    let needsInout: Bool = {
                        if methodParams.keys.contains(where: { $0 == paramNames }) { return false }
                        if letDeclTypeByToken.keys.contains(where: { paramNames.isEqual(to: $0) }) { return false }
                        return true
                    }()

                    // Нужен ли `@escaping`? — если тип — замыкание и оно либо локально объявлено,
                    // либо исходная сигнатура уже указывала `@escaping`.
                    var needsEscaping = false
                    var hadEscapingInSignature = false
                    if let attrType = paramType.as(AttributedTypeSyntax.self), let attrs = attrType.attributes {
                        hadEscapingInSignature = attrs.contains { $0.as(AttributeSyntax.self)?.attributeName.text == "escaping" }
                    }
                    if paramType.as(FunctionTypeSyntax.self) != nil && (declaredInBody || hadEscapingInSignature) {
                        needsEscaping = true
                    }

                    // Оборачиваем тип в AttributedType + inout/@escaping по необходимости.
                    let attributed: AttributedType
                    if let attrType = paramType.as(AttributedTypeSyntax.self) {
                        let withEscaping = needsEscaping ? attrType.withAttributes([escapingAttr]) : attrType
                        attributed = needsInout ? withEscaping.withSpecifier(.inout.withTrailingTrivia(.space)) : withEscaping.withTrailingTrivia(.space)
                    } else {
                        attributed = AttributedType(
                            specifier: needsInout ? .inout : nil,
                            attributes: needsEscaping ? [escapingAttr] : nil,
                            baseType: paramType.withLeadingTrivia(.space)
                        )
                    }

                    let trailingComma = ordinal < mergedParams.count ? TokenSyntax.comma : nil
                    let paramDecl = FunctionParameterSyntax(
                        leadingTrivia: ordinal == 1 ? nil : .space,
                        firstName: paramNames.firstParameter,
                        secondName: paramNames.secondParameter,
                        colon: .colon.withTrailingTrivia(.space),
                        type: attributed.withoutTrailingTrivia(),
                        trailingComma: trailingComma
                    )
                    helperParamList.append(paramDecl)
                    ordinal += 1
                }

                let helperParams = FunctionParameterListSyntax(helperParamList)

                // Если выносим последнюю инструкцию, helper может возвращать тип исходного метода.
                var helperReturn: ReturnClauseSyntax? = nil
                if statements.count == index + 1 { helperReturn = method.signature.output }

                // Добавляем @inline(never), чтобы оптимизатор не инлайнил назад.
                let inlineNever: AttributeListSyntax.Element = {
                    AttributeSyntax(
                        atSignToken: .atSign.withLeadingTrivia([.newlines(1), .spaces(4)]),
                        attributeName: .identifier("inline"),
                        leftParen: .leftParen,
                        argument: .token(.identifier("never")),
                        rightParen: .rightParen,
                        trailingTrivia: .space
                    ).as(AttributeListSyntax.Element.self)!
                }()

                // Сохраняем существующие атрибуты и добавляем inline(never).
                let attributes: AttributeListSyntax = {
                    if let existing = method.attributes {
                        return existing.count > 0 ? existing.inserting(inlineNever, at: 0) : existing.appending(inlineNever)
                    } else {
                        return AttributeListSyntax([inlineNever])
                    }
                }()

                // Копируем модификаторы, нормализуем доступ до `private`, убираем `override`.
                var modifiers: ModifierListSyntax = method.modifiers?.withTrailingTrivia(.spaces(1)).withLeadingTrivia(.spaces(1)) ?? ModifierListSyntax()
                var indexesToRemove: [Int] = []
                var haveAccessModifier = false
                for (i, mod) in modifiers.enumerated() {
                    guard let m = mod.as(DeclModifierSyntax.self) else { continue }
                    switch m.name.text {
                    case "override":
                        indexesToRemove.append(i)
                    case "public", "open", "internal":
                        haveAccessModifier = true
                        modifiers = modifiers.replacing(childAt: i, with: ModifierListSyntax.Element(leadingTrivia: .spaces(1), name: .private, trailingTrivia: .spaces(1)))
                    case "private", "fileprivate":
                        haveAccessModifier = true
                    default: break
                    }
                }
                if !haveAccessModifier {
                    modifiers = modifiers.appending(ModifierListSyntax.Element(leadingTrivia: .spaces(1), name: .private, trailingTrivia: .spaces(1)))
                }
                for idx in indexesToRemove.reversed() { modifiers = modifiers.removing(childAt: idx) }

                // Тело helper-функции: исходная инструкция, вынесенная в новый метод.
                let helperBody = Service.makeBody(
                    initialCodeBlockItemList: CodeBlockItemListSyntax([
                        item.withTrailingTrivia([.newlines(1), .spaces(8)]).withLeadingTrivia([.newlines(1), .spaces(4)])
                    ])
                ).withTrailingTrivia([.newlines(1), .spaces(4)])

                // Собираем объявление helper-функции.
                let helperFunc: FunctionDeclSyntax = Service.makeFunc(
                    modifiers: modifiers,
                    attributes: attributes,
                    keyword: .funcKeyword().withLeadingTrivia(.spaces(1)).withTrailingTrivia(.spaces(1)),
                    genericParameterClause: method.genericParameterClause,
                    identifier: newFuncIdent,
                    parameters: helperParams,
                    throwable: method.signature.throwsOrRethrowsKeyword != nil,
                    returnType: helperReturn,
                    whereClause: method.genericWhereClause,
                    body: helperBody
                )

                // Генерируем вызов helper-функции в исходном методе.
                var callArgs: [TupleExprElementSyntax] = []
                for (i, param) in helperParamList.enumerated() {
                    let external = ParameterNames(firstParameter: param.firstName, secondParameter: param.secondName)

                    // Для изменяемых локалов добавляем префикс `&`.
                    let needsAddress: Bool = {
                        let isMethodParam = methodParams.contains { $0.key == external }
                        
                        if let currentMethodParameter = methodParams.first(where: { key, value in
                            key == external
                        }), let attributedType = currentMethodParameter.value?.as(AttributedTypeSyntax.self),
                           attributedType.specifier?.text == "inout" {
                            return true
                        }
                        
                        let isLetLocal = letDeclTypeByToken.contains { external.isEqual(to: $0.key) }
                        let isBodyVar = declTypeByToken.contains { external.isEqual(to: $0.key) }
                        if isMethodParam && !isBodyVar { return false }
                        if isLetLocal { return false }
                        return true
                    }()

                    // Обработка кейса с ключевыми словами как именами (пример: `switch`).
                    let nameText = external.text == "switch" ? "`switch`" : external.text
                    let exprLiteral = (needsAddress ? "&" : "") + nameText

                    var useLabel = false
                    if let first = param.firstName?.text, first != "_" { useLabel = true }

                    var tuple = TupleExprElementSyntax(
                        label: useLabel ? param.firstName?.text : nil,
                        expression: ExprSyntax(stringLiteral: exprLiteral)
                    )
                    if helperParamList.count > 1 && i + 1 < helperParamList.count {
                        tuple = tuple.withTrailingComma(.comma).withTrailingTrivia(.space)
                    }
                    callArgs.append(tuple)
                }

                let callee = IdentifierExprSyntax(identifier: newFuncIdent)
                let call = FunctionCallExprSyntax(
                    leadingTrivia: [.newlines(1), .spaces(8)],
                    calledExpression: ExprSyntax(callee),
                    leftParen: .leftParenToken(),
                    argumentList: TupleExprElementListSyntax(callArgs),
                    rightParen: .rightParenToken(),
                    trailingTrivia: [.newlines(1), .spaces(4)]
                )

                // Если исходный метод «throws», оборачиваем вызов в `try`.
                let maybeTry: ExprSyntaxProtocol = (method.signature.throwsOrRethrowsKeyword != nil)
                    ? TryExprSyntax(leadingTrivia: [.newlines(1), .spaces(4)], expression: call)
                    : call

                var outlinedItem = CodeBlockItemSyntax(item: .init(maybeTry)).withTrailingTrivia(.newlines(1))

                // Если это последняя инструкция и метод возвращает значение — добавим `return`.
                if index == statements.count - 1, method.signature.output != nil {
                    let ret = ReturnStmtSyntax(
                        leadingTrivia: [.newlines(1), .spaces(4)],
                        returnKeyword: .return.withLeadingTrivia(.spaces(4)).withTrailingTrivia(.spaces(1)),
                        expression: ExprSyntax(maybeTry)
                    )
                    outlinedItem = outlinedItem.withItem(.init(ret)).withTrailingTrivia(.newlines(1))
                }

                // В новое тело исходного метода добавляем вызов helper-функции.
                replacedItems.append(outlinedItem)

                // Новый helper как отдельный член класса.
                members = members.appending(
                    MemberDeclListSyntax.Element(
                        leadingTrivia: [.newlines(1), .spaces(4)],
                        decl: helperFunc,
                        trailingTrivia: .newlines(1)
                    )
                )
            }

            // 8) Собираем новое тело исходного метода из накопленных элементов.
            let newBody = Service.makeBody(initialCodeBlockItemList: CodeBlockItemListSyntax(replacedItems))
                .withTrailingTrivia([.newlines(1), .spaces(4)])
            let rewrittenMethod = method.withBody(newBody)
            
            if let idx = members.firstIndex(where: { mem in
                guard let f = mem.decl.as(FunctionDeclSyntax.self) else { return false }
                return "\(f.identifier)\(f.signature)" == "\(method.identifier)\(method.signature)"
            }) {
                let rawIndex = members.distance(from: members.startIndex, to: idx)
                members = members.replacing(
                    childAt: rawIndex,
                    with: MemberDeclListSyntax.Element(
                        leadingTrivia: [.newlines(1), .spaces(4)],
                        decl: rewrittenMethod,
                        trailingTrivia: .newlines(1)
                    )
                )
            }
        }

        let newMembers = MemberDeclBlockSyntax(members: members)
        return super.visit(node.withMembers(newMembers))
    }
}

// MARK: - Service helpers

/// Набор утилит для генерации частей синтаксического дерева (тело функции, объявление функции).
enum Service {
    /// Оборачивает список инструкций в `{ ... }` с единообразными отступами и переводами строк.
    static func makeBody(initialCodeBlockItemList: CodeBlockItemListSyntax) -> CodeBlockSyntax {
        CodeBlockSyntax(
            leftBrace: .leftBraceToken(),
            statements: initialCodeBlockItemList,
            rightBrace: .rightBraceToken(),
            trailingTrivia: .newlines(1)
        )
    }

    /// Конструирует декларацию функции с переданными параметрами и телом.
    static func makeFunc(
        modifiers: ModifierListSyntax? = nil,
        attributes: AttributeListSyntax? = nil,
        keyword: TokenSyntax = .funcKeyword().withLeadingTrivia(.spaces(1)).withTrailingTrivia(.spaces(1)),
        genericParameterClause: GenericParameterClauseSyntax? = nil,
        identifier: TokenSyntax,
        parameters: FunctionParameterListSyntax = FunctionParameterListSyntax([]),
        throwable: Bool = false,
        returnType: ReturnClauseSyntax? = nil,
        whereClause: GenericWhereClauseSyntax? = nil,
        body: CodeBlockSyntax? = nil,
        indent: Int = 2
    ) -> FunctionDeclSyntax {
        let funcBody: CodeBlockSyntax = body ?? CodeBlockSyntax(
            leftBrace: .leftBraceToken(),
            statements: CodeBlockItemListSyntax([]),
            rightBrace: .rightBraceToken().withTrailingTrivia(.newlines(1)),
            trailingTrivia: .newlines(1)
        )

        let signature = FunctionSignatureSyntax(
            input: ParameterClauseSyntax(
                leftParen: .leftParenToken(),
                parameterList: parameters,
                rightParen: .rightParenToken(),
                trailingTrivia: .spaces(1)
            ),
            asyncOrReasyncKeyword: nil,
            throwsOrRethrowsKeyword: throwable ? .throws : nil,
            output: returnType
        )

        var fd = FunctionDeclSyntax(
            leadingTrivia: [.newlines(1), .spaces(4)],
            attributes: attributes,
            modifiers: modifiers,
            funcKeyword: keyword,
            identifier: identifier,
            genericParameterClause: genericParameterClause,
            signature: signature,
            genericWhereClause: whereClause,
            body: funcBody
        )
        if indent > 0 { fd = fd.withLeadingTrivia(.spaces(indent)) }
        return fd
    }
}

// MARK: - Вспомогательные типы и визиторы

/// Пара имён параметров (первое/внешнее и второе/внутреннее). Нужна для корректной
/// сборки сигнатур и сопоставления идентификаторов с параметрами.
struct ParameterNames: Hashable {
    var firstParameter: TokenSyntax?
    var secondParameter: TokenSyntax?

    init(firstParameter: TokenSyntax? = nil, secondParameter: TokenSyntax? = nil) {
        self.firstParameter = firstParameter
        self.secondParameter = secondParameter
    }

    /// Текстовое имя, которое участвует в передаче аргумента (внешнее имя или единственное имя).
    var text: String {
        if firstParameter != nil, let second = secondParameter { return second.text }
        if let first = firstParameter { return first.text }
        return ""
    }

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.firstParameter?.text == rhs.firstParameter?.text && lhs.secondParameter?.text == rhs.secondParameter?.text
    }

    /// Сопоставление с конкретным токеном идентификатора.
    func isEqual(to token: TokenSyntax) -> Bool {
        if firstParameter != nil, let second = secondParameter { return second.text == token.text }
        if let first = firstParameter { return first.text == token.text }
        return false
    }
}

/// Визитор, который помечает «опасные» вложенные конструкции внутри узла (вложенные функции,
/// типы, defer и т.д.) — такие узлы выносить нельзя, чтобы не сломать семантику.
final class BadlyInsideVisitor: SyntaxVisitor {
    var hasBadlyInside = false
    var hasReturnInside = false

    override func visit(_ node: ReturnStmtSyntax) -> SyntaxVisitorContinueKind { hasReturnInside = true; return .visitChildren }
    override func visit(_ node: FunctionDeclSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
    override func visit(_ node: ClassDeclSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
    override func visit(_ node: StructDeclSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
    override func visit(_ node: EnumDeclSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
    override func visit(_ node: PackExpansionTypeSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
    override func visit(_ node: TypealiasDeclSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
    override func visit(_ node: DeferStmtSyntax) -> SyntaxVisitorContinueKind { hasBadlyInside = true; return .visitChildren }
}

/// Визитор для сбора замыканий внутри узла. Нужен для проверки на захват изменяемых переменных.
final class ClosureVisitor: SyntaxVisitor {
    var closureList: [ClosureExprSyntax] = []
    override func visit(_ node: ClosureExprSyntax) -> SyntaxVisitorContinueKind { closureList.append(node); return .visitChildren }
}

/// Сервис получения типа идентификатора через SourceKit. Возвращает `nil`, если тип не найден
/// или признан недостоверным (например, `<<error type>>`).
enum TypeCheckService {
    static func getType(pattern: IdentifierPatternSyntax, filePath: String) -> TypeSyntax? {
        let offset = pattern.positionAfterSkippingLeadingTrivia.utf8Offset
        let request: SourceKitObject = [
            "key.request": UID("source.request.cursorinfo"),
            "key.sourcefile": filePath,
            "key.offset": offset,
            "key.compilerargs": ["-sdk", sdkPath(), "-target", "x86_64-apple-macosx10.15", filePath]
        ]
        if let response = try? Request.customRequest(request: request).send(),
           let typeName = response["key.typename"] as? String {
            guard typeName != "<<error type>>" && !typeName.contains("_") else { return nil }
            return TypeSyntax(stringLiteral: typeName)
        }
        return nil
    }
}

/// Визитор для проверки, что внутри рассматриваемого узла упоминаются изменяемые переменные (var)
/// из списка `variableList`. Если да — такую инструкцию лучше не выносить.
final class VariableVisitor: SyntaxVisitor {
    var variableList: [TokenSyntax] = []
    var haveIntersect = false

    override func visit(_ node: TokenSyntax) -> SyntaxVisitorContinueKind {
        guard !haveIntersect else { return .skipChildren }
        for tok in variableList where tok.text == node.text { haveIntersect = true; break }
        return .visitChildren
    }
}

/// Визитор, который помечает «плохие» типы, при обнаружении которых вынос лучше отменить.
/// Для простоты считаем проблемными `Set` и всё, что содержит `Any` в имени.
final class BadTypeVisitor: SyntaxVisitor {
    var skip = false
    override func visit(_ node: SimpleTypeIdentifierSyntax) -> SyntaxVisitorContinueKind {
        if node.name.text == "Set" || node.name.text.contains("Any") { skip = true }
        return .visitChildren
    }
}
