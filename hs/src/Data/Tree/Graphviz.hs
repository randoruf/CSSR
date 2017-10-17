{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree.Graphviz where

import CSSR.Prelude

import System.Directory (canonicalizePath)

import Data.GraphViz.Attributes.Complete (GraphSize(GSize), Attribute(..), Ratios(FillRatio), Shape(Circle), Label(StrLabel))
import Data.GraphViz.Printing (renderDot, toDot)
import Data.GraphViz.Types.Generalised (DotGraph, GraphID(Str))
import Data.GraphViz.Types.Monadic (digraph, graphAttrs, nodeAttrs, edgeAttrs, edge, DotM)

import qualified Data.Text.Lazy as T

main :: IO ()
main = do
  fp <- canonicalizePath "../AUTHORS.md"
  putStrLn . T.unpack . render $ mkGraphViz fp [("a", "a", 0.999),("b", "a", 0.123),("c", "b", 0.432)]

render :: DotGraph LText -> LText
render = renderDot . toDot

-- TODO:
--
-- For labels:
--   if (stateLabels) {
--     String.valueOf(i).map(c => (c.toInt + 17).toChar)
--   } else {
--     String.valueOf(i)
--
mkGraphViz :: String -> [(LText, LText, Float)]-> DotGraph LText
mkGraphViz name ts =
  digraph (Str . T.pack $ name) $ do
    graphAttrs [ Ratio FillRatio, Size (GSize 6 (Just 8.5) False) ]
    nodeAttrs  [ FontSize 24, Shape Circle ]
    edgeAttrs  [ FontSize 24 ]
    mapM buildEdge ts
 where
    buildEdge :: (LText, LText, Float) -> DotM LText ()
    buildEdge (f,t,p) = edge f t [ Label . StrLabel $ label f p ]

    label :: LText -> Float -> LText
    label f p = f <> ": " <> T.pack (f'4 p)

    -- TODO: for buildEdge, ensure parity with:
    --
    -- val dotInfo: String = dotMeta + allStates.states
    --   .zipWithIndex
    --   .map {
    --     case (state, i) =>
    --       val sTransitions:Map[Event, TransitionState] = allStates.transitionMap(state)
    --       state.distribution
    --         .toArray
    --         .view.zipWithIndex
    --         .foldLeft[String]("") {
    --         case (memo, (prob, k)) if prob <= 0 => memo
    --         case (memo, (prob, k)) =>
    --           val symbol:Event = alphabet.raw(k)
    --           sTransitions(symbol) match {
    --             case None => memo
    --             case Some(transition) =>
    --               memo + s"""${idxAsStr(i)} -> ${idxAsStr(allStates.stateMap(transition))} [label = "$symbol: ${"%.7f".format(prob)}"];\n"""
    --           }
    --       }
    --   }
    --   .reduceLeft(_+_) + "}\n\n"
