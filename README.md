# ErlangDC R13B Nerves Demo

This project contains the source code and schematics for the
BeagleBone/Nerves demo at ErlangDC R13B. It shows how to
integrate a simple circuit of LEDs and a push button switch
in a cross-compiled Erlang setup. The push button and LEDs
are accessible via the web using Cowboy and Websockets.

## Hardware

See the fritzing folder for pictures of the schematic and a
breadboard view of the wiring. The Fritzing file is included.

## Building the firmware image

Import the nerves-sdk environment and run make. You'll get a
firmware image that can be copied to an SDCard and inserted
into a BeagleBone Black. The web interface is at
http://192.168.1.40:8080.
