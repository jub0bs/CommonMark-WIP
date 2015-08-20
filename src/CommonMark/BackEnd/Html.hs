{-# LANGUAGE OverloadedStrings #-}

module CommonMark.BackEnd.Html
    ( def
    , renderBlock
    , renderDoc
    , dummyDoc
    , HtmlOptions
    ) where

import Data.Default
import Data.Monoid ( (<>), mconcat)
import Data.Sequence
import Data.Foldable ( toList )
import Data.Text ( Text )

import Text.Blaze.Html5 ( Html )
import qualified Text.Blaze.Html5 as H

import CommonMark.Types


data HtmlOptions = HtmlOptions
    { blocksep  :: Text
    , itemsep   :: Text
    , endofline :: Text
    } deriving (Show)


instance Default HtmlOptions where
    def = let nl = "\n" in HtmlOptions nl nl nl

renderDoc :: HtmlOptions -> Doc -> Html
renderDoc opts (Doc _ blocks) = renderBlocks opts blocks <> eol
  where
    eol = H.toHtml $ endofline opts


renderBlocks :: HtmlOptions -> Blocks -> Html
renderBlocks opts = mconcat . map (renderBlock opts) . toList


renderBlock :: HtmlOptions -> Block -> Html
renderBlock opts block =
    case block of
        Hrule                  -> H.hr
        Header n t
            | 1 <= n && n <= 6 -> headers !! (n - 1) $ H.toHtml t
            | otherwise        -> error "illegal header level"
        -- header parsers should be implemented in such a way that only
        -- legal level values (1 to 6) find their way into the doc's AST
        CodeBlock info t       -> H.pre . H.code . H.toHtml . (<> eol) $ t
        -- HtmlBlock t            -> undefined
        -- Paragraph t            -> undefined
        -- Blockquote blocks      -> undefined
        -- List tight ltype items -> undefined
        _ -> error "invalid block"
  where
    headers  = [H.h1, H.h2, H.h3, H.h4, H.h5, H.h6]
    bsep = blocksep opts
    isep = itemsep opts
    eol  = endofline opts


-- for testing purposes
dummyDoc :: Doc
dummyDoc = Doc def (fromList [Hrule])
