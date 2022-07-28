{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
	name = "My Shell";
	buildInputs = with pkgs; [
		SDL2
		SDL2_image
		SDL2_ttf
		SDL2_mixer
		SDL2_net
		libGL
		pkgconfig
		tmux
		sbcl
		asdf
		file
	];

	shellHook = ''
		clear;
		echo "Loaded!";
		export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath [
			pkgs.SDL2
			pkgs.SDL2_image
			pkgs.SDL2_ttf
			pkgs.SDL2_mixer
			pkgs.SDL2_net
			pkgs.libGL
		]}";
		export PS1='\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] ';
	'';
}
