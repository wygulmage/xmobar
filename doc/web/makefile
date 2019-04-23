index.src = ../../readme.md
index = readme.md
releases = ../../changelog.md
css = xmobar.css
images = ../xmobar-sawfish.png ../xmobar-xmonad.png
remote = root@xmobar.org:/var/www/xmobar.org/
htmls = index.html releases.html
title = % xmobar - a minimalistic status bar

default: index.html

$(index): $(index.src)
	@tail -n+3 $(index.src) | sed "1s/^/$(title)\n/" > $(index)

index.html: releases.html $(index) $(css)
	pandoc -f markdown -t html -c $(css) --toc -N -s \
               $(index) > index.html

releases.html: $(releases) $(css)
	pandoc -f markdown -t html -c $(css) -s \
               $(releases) > releases.html

imgs:
	cp $(images) .

upload-images: imgs
	rsync -zav $(images) $(remote)

upload-pages: $(htmls)
	scp $(htmls) $(css) $(remote)

upload: upload-images upload-pages

clean:
	rm -f $(htmls) $(index) *.png
