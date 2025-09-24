//
//  Error.swift
//  podlodkacli
//
//  Created by Павел Каретников on 20.09.2025.
//

// Кастомные ошибки
enum CustomError: Error {
    // Некорректный путь в inputPath
    case filePathError(String)
    // Неизвестный mode в --mode
    case unknownModeError(String)
}
