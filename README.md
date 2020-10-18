# rPi_DAQ

This repo contains the code for a custom bare metal OS to run Raspberry Pi as a high frequency data acquisition system, as described in the paper: "Data acquisition system for acoustic field measurements under harsh conditions"

How to Use: simply place the files in the OS folder onto a bootable SD Card and place it in your rasperry pi. The raspberry pi must be equipped with the PCB that is described in the paper
The OS is implemented with Ultibo (https://ultibo.org). In order to be able to make changes to the code, ultibo must be installed.
After compilation, replace the kernel7.img on your sd card or put it in the root directory of a USB memory stick that can then be connected to the raspberry pi. If a running version of the OS is already installed on the SD card, the new OS file will be moved to the SD card and the system will be rebooted automatically.
