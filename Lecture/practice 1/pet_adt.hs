data Pet = Dog | Cat | Bird | Fish deriving (Show, Eq)

speak :: Pet -> String
speak Dog = "Ruff!"
speak Cat = "Meow!"
speak Bird = "Chirp!"
speak Fish = "Bubble!"