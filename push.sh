cp verilog/*/*.v enlab-build-env/src
rsync -r enlab-build-env/ enlab:~/hack-usu
ssh enlab "cd ~/hack-usu; make implement"
scp enlab:~/hack-usu/Basys3_Master.bit ~/usu/ECE3700.bit
bitupload
