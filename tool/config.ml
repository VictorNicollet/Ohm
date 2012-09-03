(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

let apache_vhost args = 
  let name = Filename.basename Path.root in
  let domain = name ^ ".local" in
  Printf.printf 
    "<VirtualHost *:80>
  ServerName %s
  FastCgiServer %s/server.real

  RewriteEngine On
  RewriteCond %s%%{REQUEST_FILENAME} !-f
  RewriteRule .* /server.real [L,QSA]

  DocumentRoot %s
  <Direcvtory %s>
    AllowOverride None
    Order allow, deny
    allow from all
  </Directory>

  ErrorDocument 500 /500.htm
</VirtualHost>\n" 
    domain Path.www Path.www Path.www Path.www

