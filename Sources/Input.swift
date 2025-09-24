final class DemoService {
    func compute(param1: String, param2: inout Int, handler: (Int) -> Void) -> Int {
        var local = param2 + 10
        if !param1.isEmpty {
            handler(local)
        }
        local += 5
        return local
    }
    
    func untouched(a: Int) {
        print(a)
    }
}
