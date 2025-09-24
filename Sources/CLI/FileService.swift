//
//  FileService.swift
//  podlodkacli
//
//  Created by Павел Каретников on 20.09.2025.
//

import Foundation

enum FileService {
    static func getSorce(stringUrl: String) -> String? {
        let url = URL(fileURLWithPath: stringUrl)
        guard url.pathExtension == "swift",
              let originalSource = try? String(contentsOf: url, encoding: .utf8) else {
            return nil
        }
        
        return originalSource
    }
}
