literatex.gif: Main.hs
	cabal v2-run -- literatex-animation --width 400 --output literatex.render.gif
	gifsicle --crop 0,1+400x60 --colors 8 --output literatex.gif literatex.render.gif
	rm literatex.render.gif
