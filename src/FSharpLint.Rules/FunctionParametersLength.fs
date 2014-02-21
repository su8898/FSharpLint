﻿(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint

/// Checks if a function is declared with more than a configurable number of parameters.
module FunctionParametersLength =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open AstVisitorBase

    // Change this to be retrieved from a config file.
    [<Literal>]
    let MaxParameters = 5

    let error i = sprintf "Functions should have less than %d parameters" i
    
    let visitor postError checkFile = 
        { new AstVisitorBase(checkFile) with

            member this.VisitLongIdentPattern(longIdentifier, identifier, constructorArguments, access, range) =  
                let lastIdent = longIdentifier.Lid.[(longIdentifier.Lid.Length - 1)]

                match constructorArguments with
                | SynConstructorArgs.Pats(patterns) when List.length patterns >= MaxParameters -> 
                    let failedPattern = patterns.[MaxParameters - 1]
                    postError failedPattern.Range (error MaxParameters)
                | _ -> ()

                Continue
        }