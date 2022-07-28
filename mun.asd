(asdf:defsystem #:mun
		:description "My Universal Nostalgic Game"
		:license "MIT"
		:author "NrdyBhu1"
		:pathname "src"
		:serial t
		:depends-on (:sdl2
			      :sdl2-image
			      :sdl2-mixer)
		:components ((:file "package")
								 (:file "utils")
								 (:file "player")
								 (:file "main")))
