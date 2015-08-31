all: manual.html

manual.html: manual.txt asciidoc.conf
	PATH=../node_modules/codemirror/bin:$(PATH) asciidoc --backend=html5 -o manual.html manual.txt
