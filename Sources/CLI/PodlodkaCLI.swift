// The Swift Programming Language
// https://docs.swift.org/swift-book
// 
// Swift Argument Parser
// https://swiftpackageindex.com/apple/swift-argument-parser/documentation

import Foundation
import ArgumentParser
import SwiftSyntax
import SwiftParser

@main
struct PodlodkaCLI: ParsableCommand {
    
    // удобная обертка для опциональных параметров типа --параметр в ArgumentParser
    @Option(
        name: .shortAndLong,
        help: """
Доступные методы обфускации:
\(Mode.allCases.map( { "\($0.rawValue)" }))
""")
    var mode: String = Mode.outlineMethods.rawValue
    
    // удобная обертка для обязательных параметров в ArgumentParser
    @Argument(
        help: "Путь до файла .swift"
    )
    var inputPath: String
    
    @Argument(
        help: "Путь преобразованного .swift файла"
    )
    var outputPath: String
    
    // Точка входа при использовании ArgumentParser
    mutating func run() throws {

        // Получаем swift код из файла
        guard let swiftSource = FileService.getSorce(stringUrl: inputPath) else {
            throw CustomError.filePathError("Не удалось получить swift код из файла: \(inputPath)")
        }
        
        // Получаем AST дерево, можно даже написать строку swift кода и скастить ее
        let originalFileSyntax = Parser.parse(source: swiftSource)

        // Можно распечатать, посмотреть. Удобно чтобы дебажить проблемные места, знать что к чему кастить
//        dump(originalFileSyntax)
        
        // Убеждаемся что поддерживаем нужный режим
        guard let typedMode = Mode(rawValue: mode) else {
            throw CustomError.unknownModeError("Неизвестный режим: \(inputPath). Cмотри --help")
        }
        
        let service: SyntaxRewriter
        
        switch typedMode {
        case .outlineMethods:
            let infoCollector = InfoCollectorVisitor(viewMode: .all)
            infoCollector.walk(originalFileSyntax)
            service = OutlineMethodsService(
                infoCollector: infoCollector,
                sourceFilePath: inputPath,
                splitPattern: "_blabla"
            )
        }
        
        // Через визитор идем по AST дереву
        let result = service.visit(originalFileSyntax)
        
        // Конвертим AST в swift код пи пишем в файл по outputPath
        try result.description.write(
            toFile: outputPath,
            atomically: true,
            encoding: .utf8
        )
    }
}
