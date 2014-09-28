install::
	-cabal sandbox hc-pkg unregister story-board-diagrams	
	cabal install
	( cd story-board-diagrams ; cabal install )
