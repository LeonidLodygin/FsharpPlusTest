# FSharpPlusTest

An example of code using FSharpPlus which generates internal errors “Undefined or unsolved type variable” during build.

## Explanation

The code consists of two custom types Vector1 and Vector2 with defined functions Map2 and Map respectively. When using map from FSharpPlus.Operators no errors occur, but when using map2 in similar cases internal errors “Undefined or unsolved type variable” appear, and their number depends on the build type.

## Repro steps

```sh
> dotnet build
```

or

```sh
> dotnet build --configuration Release
```

(debug and release have different error counts)

## Related information

`dotnet SDK` 8.0.203

`FSharpPlus` 1.6.1


