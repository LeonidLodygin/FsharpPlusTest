module main
    
type Vector1<'t>() =
    static member Map2 (vector1:Vector1<'t>,vector2:Vector1<'t>, f: 't -> 't -> 't)=
        Vector1<'t>()              

let inline f () = FSharpPlus.Operators.map2 (*)

//Problem spot!!!  
let m1 () : Vector1<int> -> Vector1<int>=
    let m:(Vector1<int> -> Vector1<int> -> Vector1<int>) = f()
    m (Vector1<_>())
    
let m2 () : Vector1<int>=
    let m:(Vector1<int> -> Vector1<int> -> Vector1<int>) = f()
    m (Vector1<_>()) (Vector1<_>())
    
type Vector2<'t>() =
    static member Map (x:Vector2<'t>, f: 't -> 't) =
        Vector2<'t>()

let inline g () = FSharpPlus.Operators.map id     
 
let m3 () : Vector2<int> -> Vector2<int>=
    let m: (Vector2<int> -> Vector2<_>)  = g()
    m 
    
let m4 () : Vector2<int>=
    let m: (Vector2<int> -> Vector2<int>) = g()
    m (Vector2<_>())            