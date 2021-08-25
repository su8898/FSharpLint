module FSharpLint.Core.Tests.Rules.Conventions.InternalValuesNames

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

let config =
    { NamingConfig.Naming = Some NamingCase.CamelCase
      Underscores = Some NamingUnderscores.AllowPrefix
      Prefix = None
      Suffix = None }
[<TestFixture>]
type TestConventionsInternalValuesNames() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(InternalValuesNames.rule config)

    [<Test>]
    member this.InternalTupleIsPascalCase() =
        this.Parse """
module Program
  let internal Cat, internal Dog = 1, 0"""

        Assert.IsTrue(this.ErrorExistsAt(3, 15))

    [<Test>]
    member this.InternalFunctionNameIsPascalCase() =
        this.Parse """
module Program
  let internal Main () = ()"""

        Assert.IsTrue(this.ErrorExistsAt(3, 15))

    [<Test>]
    member this.VariablePatternMatchIsCamelCase() =
        this.Parse """
module Program
  let main =
    match true with
    | dog -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PatternMatchAsIsPascalCase() =
        this.Parse """
module Program
  let main =
    match true with
    | _ as Dog -> ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 11))

    [<Test>]
    member this.PatternMatchAsIsCamelCase() =
        this.Parse """
module Program
  let main =
    match true with
    | _ as dog -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.FunctionNameNestedInBindingIsPascalCase() =
        this.Parse """
module program
  let main () =
    let Main () = ()
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))
        
    [<Test>]
    member this.FunctionNameNestedInBindingIsCamelCase() =
        this.Parse """
module Program
  let main () =
    let bain () = ()
    ()"""

        this.AssertNoWarnings()
    [<Test>]
    member this.CamelCaseLetBindingInType() =
        this.Parse """
module Program

type Dog() =
    let cat() = ()

    member this.Goat() = ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.PascalCaseLetBindingInType() =
        this.Parse """
module program

type Dog() =
    let Cat() = ()

    member this.Goat() = ()"""

        Assert.IsTrue(this.ErrorExistsAt(5, 8))

    [<Test>]
    member this.PascalCaseLetBindingInMethod() =
        this.Parse """
module Program

type Cat() =
  member this.ContainsBinding() =
    let Goat = 0
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(6, 8))

    [<Test>]
    member this.LiteralPatternMatchExpectNoErrors() =
        this.Parse """
module Program
  [<Literal>]
  let Dog = true

  let main =
    match true with
    | Dog -> ()
    | _ -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``Lower case international characters recognised by camelCase rule``() =
        this.Parse """
module Program

let foo () =
    let żcieżka = 0
    ()
        """

        this.AssertNoWarnings()

    [<Test>]
    member this.``Quick fix for camel case converts the first character of the identifier to lower case.``() =
        let source = """
module Program

let internal foo X = 0
"""

        let expected = """
module Program

let internal foo x = 0
"""

        this.Parse source
        Assert.AreEqual(expected, this.ApplyQuickFix source)

    [<Test>]
    member this.UnionCaseInBindingContainingPascalCaseValueGeneratesWarning() =
        this.Parse("""
module Program

type SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU MyInt) = (SingleCaseDU 5)""")

        Assert.IsTrue(this.ErrorsExist)

