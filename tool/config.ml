(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

let apache_vhost args = 
  let name = Filename.basename Path.root in
  let domain = name ^ ".local" in
  Printf.printf 
    "<VirtualHost *:80>
  ServerName %s
  FastCgiServer %s/server

  RewriteEngine On
  RewriteCond %s%%{REQUEST_FILENAME} !-f
  RewriteRule .* /server [L,QSA]

  DocumentRoot %s
  <Directory %s>
    AllowOverride None
    Order allow,deny
    allow from all
  </Directory>

  ErrorDocument 500 /500.htm
</VirtualHost>\n" 
    domain Path.www Path.www Path.www Path.www

let nginx_vhost args = 
  let name = Filename.basename Path.root in
  let domain = name ^ ".local" in
  Printf.printf "server {
  listen 80;
  server_name %s;
  root %s;

  location @fastcgi {
    include /etc/nginx/fastcgi_params;
    fastcgi_pass unix:%s/socket;
  }

  location / {
    try_files $uri @fastcgi;
  }
}"
    domain Path.www Path.www 

let vhost = function
  | "apache" :: args -> apache_vhost args
  | "nginx" :: args -> nginx_vhost args 
  | _ -> print_endline "Unknown vhost target, specify 'apache' or 'nginx'"
