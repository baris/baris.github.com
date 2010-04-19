
open Json_io
open Json_type.Browse
open Http_client.Convenience

let usage_msg = 
  "\nUsage: github_projects -u USER\n"

let github_user = ref ""
let optionspeclist = 
  [ 
    ("-u", Arg.Set_string (github_user), "Github username")
  ]

let page_header_templ user =
  let title = user ^ "' github projects" in
  "<html>" ^
  "<head><title> " ^ title ^ "</title>" ^
  "<link href=\"reset.css\" rel=\"stylesheet\" type=\"text/css\" />" ^
  "<link href=\"baris.css\" rel=\"stylesheet\" type=\"text/css\" /></head>" ^
  "<body><div class=\"projects\"><h1>" ^ title ^ "</h1>"

let page_footer_templ = "</div></body></html>"

let project_templ name url desc = 
  "<a href=" ^ url ^ " class=\"project\">" ^ "<h2>" ^ name ^ "</h2>" ^ "<p>" ^ desc ^ "</p>" ^ "</a>"

let get_field_from json name =
  List.hd
    (List.map (fun (x,y) -> string y)
       (List.filter (fun (x,y) -> x = name) (objekt json)))

let output_repo out repo =
  let name = get_field_from repo "name" in
  let url = get_field_from repo "url" in
  let description = get_field_from repo "description" in
  output_string out (project_templ name url description)

let projects_page user = 
  let api_url = "http://github.com/api/v2/json" in
  let repos_url = Neturl.join_path [api_url; "repos/show"; user] in
  let repos_json = http_get repos_url in
  let tbl = make_table (objekt (json_of_string repos_json)) in
  let repos = array (field tbl "repositories") in
  let out = open_out "projects.html" in
  let rec output_repos repos = 
    match repos with
      [] -> ()
    | r::repos' -> (output_repo out r; (output_repos repos')) in

  output_string out (page_header_templ user);
  output_repos repos;
  output_string out page_footer_templ

let _ =
  Arg.parse optionspeclist (fun x -> ()) usage_msg;
  projects_page !github_user
