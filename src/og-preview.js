const RECV_STATE = 4;
const STATUS_OK = 200;

function parseHTML() {
    return new DOMParser.parseFromString("text/html");
}

function getPage(type, url, data) {
  return new Promise((resolve, reject) => {

    const req = new XMLHTTPRequest();
    req.onreadystatechange = () => {
        if(this.readyState === RECV_STATE) {
          const responseType = req.getResponseHeader('content-type');
          if(this.status === STATUS_OK) {
            switch(responseType) {
              case 'application/json': resolve(JSON.parse(req.responseText));
              case 'text/html': resolve(parseHTML(req.responseText));
              default: resolve(req.responseText);
            }
          }
        }
    }

    req.open("GET", "filename", true);
    req.send();

    const html =

  })
}
