#!/bin/bash
cp verilog/*/*.v enlab-build-env/src
rsync -r enlab-build-env/ enlab:~/hack-usu
ssh enlab "cd ~/hack-usu; PATH=\"/ece/vivado/Vivado/2024.2/bin:$PATH\" make implement"
scp enlab:~/hack-usu/Basys3_Master.bit ~/usu/ECE3700.bit
bitupload
