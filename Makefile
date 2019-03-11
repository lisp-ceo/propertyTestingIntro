default:
	cabal new-run propertyTestingIntro
specs:
	cabal new-run spec
coin_spec:
	cabal exec ghci spec/CoinSpec.hs
list_spec:
	cabal exec ghci spec/ListSpec.hs
