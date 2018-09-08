// Sample text: What a Wonderful World
// http://en.wikipedia.org/wiki/What_a_Wonderful_World
open System
let sample = """
I see trees of green, red roses, too,
I see them bloom, for me and you
And I think to myself
What a wonderful world.
I see skies of blue, and clouds of white,
The bright blessed day, the dark sacred night
And I think to myself
What a wonderful world.
The colors of the rainbow, so pretty in the sky,
Are also on the faces of people going by.
I see friends shaking hands, sayin', "How do you do?"
They're really sayin', "I love you."
I hear babies cryin'. I watch them grow.
They'll learn much more than I'll ever know
And I think to myself
What a wonderful world
Yes, I think to myself
What a wonderful world"""

// just bigrams, simple approach

// break a string into "words"

let separators = [|' '; '\n' |]
let wordify (text:string) = 
    text.Split separators 
    |> Array.filter (fun word -> word <> "")
    
// group sequence of words into bigrams

let ngramify (n:int) (words:string[]) = words |> Seq.windowed n |> Seq.toList

// find all the bigrams that begin with a given word
// and return all the possible "next words"

let followingWords (words:string list) (ngrams:string list seq) =
    ngrams
    |> Seq.filter (fun bigram -> 
        let bi = List.toArray bigram
        List.mapi (fun i w->w=bi.[i]) words
        |> List.forall (fun x ->x=true))
    |> Seq.map (fun bigram -> Seq.tail bigram)
    |> Seq.toList

// pickup a random next word from possible ones.
// if no candidate is found, return None.

let rng = System.Random ()
let nextWord ngrams words =
    let candidates =        
        ngrams
        |> followingWords words
    match candidates with
    | [] -> None
    | _ -> 
        let index = rng.Next(candidates.Length)
        candidates.[index] |> Seq.tryLast

// given a text sample and a starting word,
// search for a next word and append it
// to a sentence, until no next word is found
// or the last word is followed by ., so
// that the results "looks like" a full sentence.
let generateFrom (start:string list) (sample:string) =
    let ngrams = 
        sample
        |> wordify
        |> ngramify (List.length start + 1)
        |> List.map (fun x->Seq.toList x)
    let next = nextWord ngrams
    let rec generate sentence words =
        match (next words) with
        | None -> sentence
        | Some(nextWord) -> 
            let sentence = sentence + " " + nextWord
            if  nextWord.EndsWith(".")
            then sentence
            else 
                let newWords = (List.tail words) @ [nextWord]
                generate sentence newWords
    let sentence = String.Join(" ", start)        
    generate sentence start

// ... see it in action
sample |> generateFrom ["I"; "see"; "trees"]