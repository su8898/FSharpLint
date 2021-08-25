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
    member this.FunctionNameNestedInBindingIsPascalCase() =
        this.Parse """
module program
  let main () =
    let Main () = ()
    ()"""

        Assert.IsTrue(this.ErrorExistsAt(4, 8))
 
    [<Test>]
    member this.LiteralPatternMatchExpectNoErrors() =
        this.Parse """
module Program
  [<Literal>]
  let internal Dog = true

  let main =
    match true with
    | Dog -> ()
    | _ -> ()"""

        this.AssertNoWarnings()

    [<Test>]
    member this.``Lower case international characters recognised by camelCase rule``() =
        this.Parse """
module Program

let internal foo () =
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

type internal SingleCaseDU = SingleCaseDU of int

let (SingleCaseDU MyInt) = (SingleCaseDU 5)""")

        Assert.IsTrue(this.ErrorsExist)

