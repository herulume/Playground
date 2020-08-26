package cyoa

import (
	"html/template"
	"net/http"
	"strings"
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
	<section class="page">
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
	  {{else}}
		<h3>The End</h3>
	  {{end}}
	</section>
	<style>
	  body {
		font-family: helvetica, arial;
	  }
	  h1 {
		text-align:center;
		position:relative;
	  }
	  .page {
		width: 80%;
		max-width: 500px;
		margin: auto;
		margin-top: 40px;
		margin-bottom: 40px;
		padding: 80px;
		background: #FFFCF6;
		border: 1px solid #eee;
		box-shadow: 0 10px 6px -6px #777;
	  }
	  ul {
		border-top: 1px dotted #ccc;
		padding: 10px 0 0 0;
		-webkit-padding-start: 0;
	  }
	  li {
		padding-top: 10px;
	  }
	  a,
	  a:visited {
		text-decoration: none;
		color: #6295b5;
	  }
	  a:active,
	  a:hover {
		color: #7792a2;
	  }
	  p {
		text-indent: 1em;
	  }
	</style>
  </body>
</html>`

type server struct {
	story    Story
	template *template.Template
}

func (s server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	path := cleanPath(r.URL.Path)

	if st, ok := s.story[path]; ok {
		err := tpl.Execute(w, st)
		if err != nil {
			http.Error(w, "Something went wrong.", http.StatusInternalServerError)
		}
		return
	}

	http.Error(w, "Chapter not found", http.StatusNotFound)
}

func NewStoryServer(s Story, t *template.Template) http.Handler {
	if t == nil {
		t = tpl
	}

	return server{s, t}
}

func cleanPath(path string) string {
	path = strings.TrimSpace(path)

	if path == "" || path == "/" {
		path = "/intro"
	}

	return path[1:] // Remove '/'
}
