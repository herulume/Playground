package cyoa

import (
	"html/template"
	"net/http"
)

func init() {
	tpl = template.Must(template.New("").Parse(defaultHandlerTmpl))
}

var tpl *template.Template

var defaultHandlerTmpl = `
<!DOCTYPE html>
<html>
  <head>
	<meta charset="utf-8">
	<title>Choose Your Own Adventure</title>
  </head>
  <body>
	  <h1>{{.Title}}</h1>
	  {{range .Paragraphs}}
		<p>{{.}}</p>
	  {{end}}
	  {{if .Options}}
		<ul>
		{{range .Options}}
		  <li><a href="/{{.Arc}}">{{.Text}}</a></li>
		{{end}}
		</ul>
	   {{end}}
  </body>
</html>`

type server struct {
	story Story
}

func (s server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	path := r.URL.Path

	if path == "" || path == "/" {
		path = "/intro"
	}
	path = path[1:] // Remove '/'

	if st, ok := s.story[path]; ok {
		err := tpl.Execute(w, st)
		if err != nil {
			http.Error(w, "Something went wrong.", http.StatusInternalServerError)
		}
		return
	}

	http.Error(w, "Chapter not found", http.StatusNotFound)
}

func NewStoryServer(s Story) http.Handler {
	return server{s}
}
