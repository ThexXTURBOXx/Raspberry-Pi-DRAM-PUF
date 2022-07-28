nix build --extra-experimental-features nix-command -f . vc4.firmware || exit
rm -f bootcode.bin
cp result/bootcode.bin .
