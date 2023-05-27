build-elm:
	elm make src/Mako.elm && sed -i -e 's/<\/title>/<\/title><script src="https:\/\/cdn.tailwindcss.com"><\/script>/g' index.html
