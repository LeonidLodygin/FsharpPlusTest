module main
open FSharpx.Collections
open System

//Test case identifying problems in FsharpPlus.
//Implementations of some functions have been omitted as unnecessary to make the code easier to read.
//The code is an attempt to implement a neural network.

//Abstraction of layer
type ILayer<'Input, 'Output> =
    abstract Forward : input: 'Input -> 'Output
    abstract Backward : input: 'Output -> 'Input

//Type for matrices
type Matrix<'t>(rows:uint, columns:uint) =
    let store = Array.init (int rows) (fun _ -> Array.zeroCreate<'t> (int columns))
    member this.Rows = rows
    member this.Columns = columns
    member this.Data = store
    static member Map (x:Matrix<'t>, f) =
        Matrix(x.Rows, x.Columns)
    static member Map2 (m1:Matrix<'t1>,m2:Matrix<'t2>, f)=
        Matrix<'c>(m1.Rows, m1.Columns)
    static member Transpose(x:Matrix<'a>) =
        Matrix(x.Columns, x.Rows)

//Type for vectors     
type Vector<'t>(store: array<'t>) =
    let store = store
    member this.Count = uint store.Length
    member this.Data = store
    static member Map (x:Vector<'t>, f) =
        Vector(x.Count)
    static member Map2 (vector1:Vector<'t1>,vector2:Vector<'t2>, f)=
        Vector<'c>(vector1.Count)              
    new (length:uint) = 
        Vector(Array.zeroCreate<'t> (int length))
        
//Some helpful functions       
let vXm (vector:Vector<'a>) (matrix:Matrix<'b>) (opMult:'a -> 'b -> 'c) (opAdd: 'c -> 'c -> 'c) =
    Vector(matrix.Columns)                 
let initMatrix rows columns f =
    Matrix<'b>(rows, columns)
let initVector (length: uint) f =
    let store = Array.init (int length) f
    Vector store     

//Linear layer    
type Linear(weights:Matrix<float>, bias:Vector<float>) =
    interface ILayer<Vector<float>, Vector<float>> with
       member this.Forward input =
            vXm input weights (*) (+)
            |> FSharpPlus.Operators.map2 (+) bias              
       member this.Backward input =
           vXm input (Matrix<float>.Transpose weights) (*) (+)
           
    new (inputSize:uint, outSize:uint) =
        let rand = Random()
        let weights = initMatrix inputSize outSize (fun _ _ -> rand.NextDouble())
        let bias = initVector outSize (fun _ -> rand.NextDouble())
        Linear(weights, bias)
              
//Abstraction of neural network        
type Model<'input,'output> () =
    let mutable firstLayer:Option<ILayer<'input,_>> = None
    let mutable lastLayer:Option<ILayer<_,'output>> = None
    let intermediateLayers = ResizeArray<ILayer<_,_>>()
        
    member this.AddLayer (l:ILayer<_,_>) =
        match firstLayer with
        | None -> firstLayer <- Some (l :?> ILayer<'input, _>)
        | Some _ -> intermediateLayers.Add lastLayer.Value        
        lastLayer <- Some (l :?> ILayer<_, 'output>)
        
let (>~>) (model:Model<'input,'output>) (layer:ILayer<'output,'newOutput>) =
    model.AddLayer layer
    model    
let empty<'T>() = new Model<'T,'T>()
 
//Abstraction of activation layer    
type Activation<'T>(activation: 'T -> 'T, derivative: 'T -> 'T -> 'T) =
    let mutable input: 'T option = None
    interface ILayer<'T, 'T> with
        member this.Forward input =
            this.SetInput input
            activation input
        member this.Backward input =
            derivative input (this.GetInput())      
    member private this.SetInput inputValue =
       input <- Some inputValue
    member this.GetInput() =
        match input with
        | Some inputValue -> inputValue
        | None -> failwith "Input value is not set"
        
//Sigmoid function with derivative        
let sigmoidFunc x =
    1.0/(1.0 + Math.Exp(-x))
let sigmoidDerivative x =
    let y = sigmoidFunc x
    y * (1.0 - y)


//Sigmoid layer
//Problem spot!!! 
let inline sigmoid () =
     let derivative backwardInput input =
         FSharpPlus.Operators.map2 (fun x y -> x * y) backwardInput (FSharpPlus.Operators.map sigmoidDerivative input)
     Activation(FSharpPlus.Operators.map sigmoidFunc, derivative)

//Simple model  
let m2 =
    let m =
        empty()
        >~> Linear(3u, 2u)
        >~> sigmoid()
        >~> Linear(2u, 2u)
        >~> sigmoid()
        >~> Linear(2u, 1u)
    printfn $"Model:\n%A{m.ToString()}"             