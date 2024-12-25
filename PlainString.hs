module PlainString where
    
-- Define Strings without Quotes for Printing
newtype PlainString = PlainString String

instance Show PlainString where
    show :: PlainString -> String
    show (PlainString str) = str