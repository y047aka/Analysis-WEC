module TypedSvg.TypesToStrings exposing (lengthToString, paintToString)

import Color exposing (toCssString)
import TypedSvg.Types exposing (Length(..), Paint(..))


lengthToString : Length -> String
lengthToString length =
    case length of
        Cm x ->
            String.fromFloat x ++ "cm"

        Em x ->
            String.fromFloat x ++ "em"

        Ex x ->
            String.fromFloat x ++ "ex"

        In x ->
            String.fromFloat x ++ "in"

        Mm x ->
            String.fromFloat x ++ "mm"

        Num x ->
            String.fromFloat x

        Pc x ->
            String.fromFloat x ++ "pc"

        Percent x ->
            String.fromFloat x ++ "%"

        Pt x ->
            String.fromFloat x ++ "pt"

        Px x ->
            String.fromFloat x ++ "px"


paintToString : Paint -> String
paintToString paint =
    case paint of
        Paint color ->
            toCssString color

        Reference string ->
            String.concat [ "url(#", string, ")" ]

        ContextFill ->
            "context-fill"

        ContextStroke ->
            "context-stroke"

        PaintNone ->
            "none"
