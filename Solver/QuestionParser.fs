module QuestionParser

open Types
open FSharp.Text.Lexing
open Lexer
open Parser
open System.Text.RegularExpressions

let parseQuestionText text =
    let lexbuf = LexBuffer<char>.FromString text
    let res = Parser.start Lexer.read lexbuf
    res

  