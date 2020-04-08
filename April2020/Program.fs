open System.Collections

let words = System.IO.File.ReadAllLines("words.txt")

let capacity = 1024*1024

let hashBit word =
  abs(hash word % capacity)

let addToBloom (bloom:BitArray) word =
  let bit = hashBit word
  bloom.[bit] <- true
  bloom

let bloom = 
  words
  |> Array.fold addToBloom (BitArray(capacity))

let isSpelledCorrectly word =
  let bit = hashBit word
  bloom.[bit]


isSpelledCorrectly "hello"
isSpelledCorrectly "filter"

// #time
// [|for i in bloom -> i|] 
// |> Seq.groupBy id 
// |> Seq.map (fun (v,a) -> v, Seq.length a)

// let _ =
//   [for i in bloom -> i] 
//   |> Seq.groupBy id 
//   |> Seq.map (fun (key,values) -> key, Seq.length values)
