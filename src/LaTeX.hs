{-# LANGUAGE OverloadedStrings #-}

module LaTeX where

import Structures
    
import Data.Ratio
import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Base.Commands

renderToFile :: LaTeX -> String -> IO ()
renderToFile latex name = renderFile name latex

makeDocument :: Calculation -> LaTeX
makeDocument calc = preamble
                 <> document (body calc)

preamble :: LaTeX
preamble = documentclass [] article
        <> usepackage [] "amsmath"
        <> author "Zachary Winkeler and Bruce Zou"
        <> title "Calculus"

body :: Calculation -> LaTeX
body calc = maketitle
         <> section "Calculation"
         <> texCalc calc

texCalc :: Calculation -> LaTeX
texCalc (Calc expr steps) = "We will attempt to simplify the following expression:"
                         <> mathDisplay (texOuterExpr expr)
                         <> foldr (<>) mempty (map texStep steps)
                         <> "This is our final answer."

texStep :: Step -> LaTeX
texStep (Step rule expr) = 
        raw (fromString ("We can use the ")) 
     <> textit (raw (fromString rule))
     <> raw (fromString (" to rewrite our expression as:"))
     <> mathDisplay (texOuterExpr expr)

texOuterExpr :: Expression -> LaTeX
texOuterExpr expr = case expr of
    BinaryOperation op expr1 expr2 -> (texBinOp op) (texExpr expr1) (texExpr expr2)
    ACOperation op exprs -> (texACOp op) (map texExpr exprs)
    _ -> texExpr expr

texExpr :: Expression -> LaTeX
texExpr expr = case expr of
    Constant m -> 
        if (denominator m) == 1 
        then texy (numerator m)
        else texy m
    Reference v -> texVar v
    BinaryOperation op expr1 expr2 -> parenthesize ((texBinOp op) (texExpr expr1) (texExpr expr2))
    ACOperation op exprs -> parenthesize ((texACOp op) (map texExpr exprs))
    Application v expr -> operatorname (texVar v) <> parenthesize (texOuterExpr expr)
    Derivative v expr -> frac totald (totald <> (texVar v)) <> parenthesize (texOuterExpr expr)

texVar :: Variable -> LaTeX
texVar (Variable name) = TeXRaw (fromString name)

texBinOp :: BinOp -> LaTeX -> LaTeX -> LaTeX
texBinOp op = case op of
    Sub -> between (TeXRaw "-")
    Div -> frac
    Pow -> (^:)

texACOp :: ACOp -> [LaTeX] -> LaTeX
texACOp op = case op of
    Add -> foldr1 (between (TeXRaw "+"))
    Mul -> foldr1 cdot

left :: LaTeX -> LaTeX
left = (TeXRaw (fromString "\\left") <>)

right :: LaTeX -> LaTeX
right = (TeXRaw (fromString "\\right") <>)

parenthesize :: LaTeX -> LaTeX
parenthesize latex = between latex (left "(") (right ")")