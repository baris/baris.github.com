USER=baris

all: projects.html

projects.html: github_projects
	./github_projects -u ${USER}

github_projects: github_projects.ml
	ocamlfind opt -package "netclient json-wheel str shell" -linkpkg github_projects.ml -o github_projects

clean:
	rm -rf github_projects{,.cmi,.cmx,.o}

clean_all: clean
	rm -rf projects.html