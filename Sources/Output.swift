final class DemoService2 {
    func compute(param1: String, param2: inout Int, handler: (Int) -> Void) -> Int {
        var local = param2 + 10
        _compute_blabla0(handler:handler, param1:param1, param2:&param2, local:&local)
        
        _compute_blabla1(local:&local, param1:param1, handler:handler, param2:&param2)
        
        return
        _compute_blabla2(param2:&param2, param1:param1, handler:handler, local:&local)
    }
    
    func untouched(a: Int) {
        _untouched_blabla0(a:a)
    }
    
    @inline(never)  private  func  _compute_blabla0(handler:  (Int) -> Void, param1:  String, param2: inout Int, local : inout Int) {
        if !param1.isEmpty {
            handler(local)
        }
    }
    
    @inline(never)  private  func  _compute_blabla1(local : inout Int, param1:  String, handler:  (Int) -> Void, param2: inout Int) {
        local += 5
    }
    
    @inline(never)  private  func  _compute_blabla2(param2: inout Int, param1:  String, handler:  (Int) -> Void, local : inout Int) -> Int {
        return local
    }
    
    @inline(never)  private  func  _untouched_blabla0(a:  Int) {
        print(a)
    }
}
