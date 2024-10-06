module {
    func.func @diff_of_squares(%x: f32, %y: f32) -> f32 {
        %x_sq = arith.mulf %x, %x : f32
        %y_sq = arith.mulf %y, %y: f32
        %r = arith.subf %x_sq, %y_sq : f32
        func.return %r: f32
    }

    func.func @main(%arg1: f32, %arg2: f32) -> f32 {
        %0 = func.call @diff_of_squares(%arg1, %arg2) : (f32, f32) -> f32
        func.return %0 : f32
    }
}
