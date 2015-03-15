# curried-functions
Curried version of all free functions in Swift standard library.
Enable you to chain free functions in Swift standard library together using `|>` operator.

## Example

```Swift
let array = [1, 2, 3, 4, 5]

// OOP feat. FP style
array
	.filter { $0 > 2 }
	.map { println($0) }

// FP style, without curried-functions
map(filter(array) { $0 > 2 }) { println($0) }

// FP style, with curried-functions
array
	|> filter { $0 > 2 }
	|> map { $0 |> println }
```

## Importing to your project

### iOS 7 or later:
Drag & drop `Curried functions.swift` to your Xcode project

### iOS 8 or later:
You can use [Carthage](https://github.com/Carthage/Carthage) to add curried-functions to your project.