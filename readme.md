# RShiny application to visualize chimical data of DYLAQ database
***

Author: David Funosas - INRAE

Date: 2021, June

Copyright: © INRAE, 2021

Application distributed under license **Open License**.

***

This is an interactive web app developed as a tool to easily access, visualize and manipulate physico-chemical data from the DYLAQ database.

## Installation

Add the content of the repository into your folder tree.

Rename the file *global_config.csv.dist* to *global_config.csv*, and edit it to change the database connection values.

For the content of the application, show the files Readme_en.md or Readme_fr.md.

## Configuration of RShiny

### List of R Packages

~~~
                  Package             LibPath
askpass           "askpass"           "/usr/local/lib/R/site-library"
assertthat        "assertthat"        "/usr/local/lib/R/site-library"
backports         "backports"         "/usr/local/lib/R/site-library"
base64enc         "base64enc"         "/usr/local/lib/R/site-library"
BH                "BH"                "/usr/local/lib/R/site-library"
blob              "blob"              "/usr/local/lib/R/site-library"
broom             "broom"             "/usr/local/lib/R/site-library"
bslib             "bslib"             "/usr/local/lib/R/site-library"
cachem            "cachem"            "/usr/local/lib/R/site-library"
callr             "callr"             "/usr/local/lib/R/site-library"
cellranger        "cellranger"        "/usr/local/lib/R/site-library"
cli               "cli"               "/usr/local/lib/R/site-library"
clipr             "clipr"             "/usr/local/lib/R/site-library"
colorspace        "colorspace"        "/usr/local/lib/R/site-library"
commonmark        "commonmark"        "/usr/local/lib/R/site-library"
cpp11             "cpp11"             "/usr/local/lib/R/site-library"
crayon            "crayon"            "/usr/local/lib/R/site-library"
crosstalk         "crosstalk"         "/usr/local/lib/R/site-library"
curl              "curl"              "/usr/local/lib/R/site-library"
DBI               "DBI"               "/usr/local/lib/R/site-library"
dbplyr            "dbplyr"            "/usr/local/lib/R/site-library"
digest            "digest"            "/usr/local/lib/R/site-library"
dplyr             "dplyr"             "/usr/local/lib/R/site-library"
DT                "DT"                "/usr/local/lib/R/site-library"
ellipsis          "ellipsis"          "/usr/local/lib/R/site-library"
evaluate          "evaluate"          "/usr/local/lib/R/site-library"
fansi             "fansi"             "/usr/local/lib/R/site-library"
farver            "farver"            "/usr/local/lib/R/site-library"
fastmap           "fastmap"           "/usr/local/lib/R/site-library"
forcats           "forcats"           "/usr/local/lib/R/site-library"
fs                "fs"                "/usr/local/lib/R/site-library"
generics          "generics"          "/usr/local/lib/R/site-library"
ggplot2           "ggplot2"           "/usr/local/lib/R/site-library"
glue              "glue"              "/usr/local/lib/R/site-library"
gridExtra         "gridExtra"         "/usr/local/lib/R/site-library"
gtable            "gtable"            "/usr/local/lib/R/site-library"
haven             "haven"             "/usr/local/lib/R/site-library"
highr             "highr"             "/usr/local/lib/R/site-library"
hms               "hms"               "/usr/local/lib/R/site-library"
htmltools         "htmltools"         "/usr/local/lib/R/site-library"
htmlwidgets       "htmlwidgets"       "/usr/local/lib/R/site-library"
httpuv            "httpuv"            "/usr/local/lib/R/site-library"
httr              "httr"              "/usr/local/lib/R/site-library"
isoband           "isoband"           "/usr/local/lib/R/site-library"
jquerylib         "jquerylib"         "/usr/local/lib/R/site-library"
jsonlite          "jsonlite"          "/usr/local/lib/R/site-library"
knitr             "knitr"             "/usr/local/lib/R/site-library"
labeling          "labeling"          "/usr/local/lib/R/site-library"
later             "later"             "/usr/local/lib/R/site-library"
lazyeval          "lazyeval"          "/usr/local/lib/R/site-library"
leaflet           "leaflet"           "/usr/local/lib/R/site-library"
leaflet.providers "leaflet.providers" "/usr/local/lib/R/site-library"
lifecycle         "lifecycle"         "/usr/local/lib/R/site-library"
lubridate         "lubridate"         "/usr/local/lib/R/site-library"
magrittr          "magrittr"          "/usr/local/lib/R/site-library"
markdown          "markdown"          "/usr/local/lib/R/site-library"
mime              "mime"              "/usr/local/lib/R/site-library"
modelr            "modelr"            "/usr/local/lib/R/site-library"
munsell           "munsell"           "/usr/local/lib/R/site-library"
openssl           "openssl"           "/usr/local/lib/R/site-library"
pillar            "pillar"            "/usr/local/lib/R/site-library"
pkgconfig         "pkgconfig"         "/usr/local/lib/R/site-library"
png               "png"               "/usr/local/lib/R/site-library"
prettyunits       "prettyunits"       "/usr/local/lib/R/site-library"
processx          "processx"          "/usr/local/lib/R/site-library"
progress          "progress"          "/usr/local/lib/R/site-library"
promises          "promises"          "/usr/local/lib/R/site-library"
ps                "ps"                "/usr/local/lib/R/site-library"
purrr             "purrr"             "/usr/local/lib/R/site-library"
R6                "R6"                "/usr/local/lib/R/site-library"
rappdirs          "rappdirs"          "/usr/local/lib/R/site-library"
raster            "raster"            "/usr/local/lib/R/site-library"
RColorBrewer      "RColorBrewer"      "/usr/local/lib/R/site-library"
Rcpp              "Rcpp"              "/usr/local/lib/R/site-library"
readr             "readr"             "/usr/local/lib/R/site-library"
readxl            "readxl"            "/usr/local/lib/R/site-library"
rematch           "rematch"           "/usr/local/lib/R/site-library"
reprex            "reprex"            "/usr/local/lib/R/site-library"
rgdal             "rgdal"             "/usr/local/lib/R/site-library"
rlang             "rlang"             "/usr/local/lib/R/site-library"
rmarkdown         "rmarkdown"         "/usr/local/lib/R/site-library"
RPostgreSQL       "RPostgreSQL"       "/usr/local/lib/R/site-library"
rstudioapi        "rstudioapi"        "/usr/local/lib/R/site-library"
rvest             "rvest"             "/usr/local/lib/R/site-library"
sass              "sass"              "/usr/local/lib/R/site-library"
scales            "scales"            "/usr/local/lib/R/site-library"
selectr           "selectr"           "/usr/local/lib/R/site-library"
shiny             "shiny"             "/usr/local/lib/R/site-library"
shiny.i18n        "shiny.i18n"        "/usr/local/lib/R/site-library"
shinydashboard    "shinydashboard"    "/usr/local/lib/R/site-library"
shinyFiles        "shinyFiles"        "/usr/local/lib/R/site-library"
shinyjs           "shinyjs"           "/usr/local/lib/R/site-library"
shinyWidgets      "shinyWidgets"      "/usr/local/lib/R/site-library"
sourcetools       "sourcetools"       "/usr/local/lib/R/site-library"
sp                "sp"                "/usr/local/lib/R/site-library"
stringi           "stringi"           "/usr/local/lib/R/site-library"
stringr           "stringr"           "/usr/local/lib/R/site-library"
sys               "sys"               "/usr/local/lib/R/site-library"
tibble            "tibble"            "/usr/local/lib/R/site-library"
tidyr             "tidyr"             "/usr/local/lib/R/site-library"
tidyselect        "tidyselect"        "/usr/local/lib/R/site-library"
tidyverse         "tidyverse"         "/usr/local/lib/R/site-library"
tinytex           "tinytex"           "/usr/local/lib/R/site-library"
utf8              "utf8"              "/usr/local/lib/R/site-library"
vctrs             "vctrs"             "/usr/local/lib/R/site-library"
viridis           "viridis"           "/usr/local/lib/R/site-library"
viridisLite       "viridisLite"       "/usr/local/lib/R/site-library"
withr             "withr"             "/usr/local/lib/R/site-library"
xfun              "xfun"              "/usr/local/lib/R/site-library"
xml2              "xml2"              "/usr/local/lib/R/site-library"
xtable            "xtable"            "/usr/local/lib/R/site-library"
yaml              "yaml"              "/usr/local/lib/R/site-library"
base              "base"              "/usr/lib/R/library"
boot              "boot"              "/usr/lib/R/library"
class             "class"             "/usr/lib/R/library"
cluster           "cluster"           "/usr/lib/R/library"
codetools         "codetools"         "/usr/lib/R/library"
compiler          "compiler"          "/usr/lib/R/library"
datasets          "datasets"          "/usr/lib/R/library"
foreign           "foreign"           "/usr/lib/R/library"
graphics          "graphics"          "/usr/lib/R/library"
grDevices         "grDevices"         "/usr/lib/R/library"
grid              "grid"              "/usr/lib/R/library"
KernSmooth        "KernSmooth"        "/usr/lib/R/library"
lattice           "lattice"           "/usr/lib/R/library"
MASS              "MASS"              "/usr/lib/R/library"
Matrix            "Matrix"            "/usr/lib/R/library"
methods           "methods"           "/usr/lib/R/library"
mgcv              "mgcv"              "/usr/lib/R/library"
nlme              "nlme"              "/usr/lib/R/library"
nnet              "nnet"              "/usr/lib/R/library"
parallel          "parallel"          "/usr/lib/R/library"
rpart             "rpart"             "/usr/lib/R/library"
spatial           "spatial"           "/usr/lib/R/library"
splines           "splines"           "/usr/lib/R/library"
stats             "stats"             "/usr/lib/R/library"
stats4            "stats4"            "/usr/lib/R/library"
survival          "survival"          "/usr/lib/R/library"
tcltk             "tcltk"             "/usr/lib/R/library"
tools             "tools"             "/usr/lib/R/library"
utils             "utils"             "/usr/lib/R/library"

~~~

### File /etc/shiny-server/shiny-server.conf

If the application is loaded in a home folder:

~~~
# Tell Shiny Server that we want to run as the user whose
# home directory we find the application in.
run_as :HOME_USER:;
preserve_logs true;
# Define a server that listens of port 3838.
server {
  listen 3838;

  # Define a location at the base URL
  location / {

    # Allow users to host their own apps in ~/ShinyApps
    user_dirs;

    # Optionally, you can restrict the privilege of hosting Shiny applications
    # only to members of a particular Linux group.
    # members_of shinyUsers;
  }
}
~~~

### Configuration of NGINX web server

#### File /etc/shiny-server/shiny-server.conf

~~~
map $http_upgrade $connection_upgrade {
  default upgrade;
  '' close;
}

server {
  listen 443;
  server_name <your_server_name>;

  ssl    on;
  ssl_session_timeout  5m;
  ssl_protocols  TLSv1.2;
  ssl_ciphers  HIGH:!aNULL:!MD5;
  ssl_prefer_server_ciphers   on;

  ssl_certificate        /etc/ssl/certs/certificate.cer;
  ssl_certificate_key    /etc/ssl/private/privatekey.key;

  access_log /var/log/nginx/shiny.log;
  error_log /var/log/nginx/shiny-error.log error;

  location / {

    proxy_set_header    Host $host;
    proxy_set_header    X-Real-IP $remote_addr;
    proxy_set_header    X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header    X-Forwarded-Proto $scheme;
    proxy_pass          http://localhost:3838;
    proxy_read_timeout  20d;
    proxy_buffering off;

    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection $connection_upgrade;
    proxy_http_version 1.1;

    proxy_redirect      / $scheme://$host/;
  }
}
