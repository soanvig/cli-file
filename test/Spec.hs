{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Test.Hspec
  import Lib (buildCommand)

  notMatchingCommand = Left "No matching command found"
  notEnoughParameters = Left "Supplied less parameters than expected. Expected argument: $param"
  tooMuchParameters = Left "Supplied more parameters than expected. Unexpected value: value"

  main :: IO ()
  main = hspec $ do
    describe "Lib.buildCommand" $ do
      context "when given empty input" $ do
        it "should error no matching command" $ do
          buildCommand "" ["test"] `shouldBe` notMatchingCommand
      
      context "when given input with no arguments" $ do
        it "should error no matching command if not found" $ do
          buildCommand "myPwd -> pwd" ["testCommand"] `shouldBe` notMatchingCommand

        it "should match command if possible" $ do
          buildCommand "myPwd -> pwd" ["myPwd"] `shouldBe` Right "pwd"

      context "when given input with required arguments" $ do
        it "should error no matching command if not found" $ do
          buildCommand "myEcho :: param -> echo $param" ["testCommand"] `shouldBe` notMatchingCommand

        it "should match command if possible" $ do
          buildCommand "myPwd :: param -> echo $param" ["myPwd", "value"] `shouldBe` Right "echo value"

        it "should match but fail if no argument given" $ do
          buildCommand "myPwd :: param -> echo $param" ["myPwd"] `shouldBe` notEnoughParameters

        it "should match but fail if too many arguments given" $ do
          buildCommand "myPwd :: param -> echo $param" ["myPwd", "value", "value"] `shouldBe` tooMuchParameters

      context "when given input with optional arguments" $ do
        it "should error no matching command if not found" $ do
          buildCommand "myEcho :: param = foobar -> echo $param" ["testCommand"] `shouldBe` notMatchingCommand

        it "should match command if possible" $ do
          buildCommand "myPwd :: param = foobar -> echo $param" ["myPwd", "value"] `shouldBe` Right "echo value"

        it "should match with default value if no parameter given" $ do
          buildCommand "myPwd :: param = foobar -> echo $param" ["myPwd"] `shouldBe` Right "echo foobar"

        it "should match but fail if too many arguments given" $ do
          buildCommand "myPwd :: param = foobar -> echo $param" ["myPwd", "value", "value"] `shouldBe` tooMuchParameters

      context "when given empty lines or comments" $ do
        it "should ignore them" $ do
          buildCommand "\n\n# comment \nmyPwd -> pwd\n # comment with space before" ["myPwd"] `shouldBe` Right "pwd"

      context "when given input with two arguments" $ do
        it "should match if possible" $ do
          buildCommand "myEcho :: first, second = World -> echo $first $second" ["myEcho", "Hello"] `shouldBe` Right "echo Hello World"
        

    