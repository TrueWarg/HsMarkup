module Component where
import Tag

makeTitleBodyItem :: [Char] -> [Char] -> [Char]
makeTitleBodyItem title_ body_ = root (head (title title_) <> body body_)