const RECV_STATE = 4;
const STATUS_OK = 200;

const FT_JSON = "application/json";
const FT_HTML = "text/html";

const REQUEST_TYPES = {
  GET: "GET",
}

function parseHTML() {
  return new DOMParser.parseFromString("text/html");
}

function request(type, url, data) {
  return new Promise((resolve, reject) => {
    const req = new XMLHttpRequest();

    req.onreadystatechange = () => {
      if(this.readyState === RECV_STATE) {
        const responseType = req.getResponseHeader('content-type');
        if(this.status === STATUS_OK) {
          // parse different types of data for you
          switch(responseType) {
            case FT_JSON: resolve(JSON.parse(req.responseText));
            case FT_HTML: resolve(parseHTML(req.responseText));
            default: resolve(req.responseText);
          }
        } else {
          reject({status: req.status, body: req.responseText });
        }
      }
    }

    req.open(type, url, true);
    req.send();
  })
}

function openGraphData(url) {
  const og_html = request(REQUEST_TYPES.GET, "https://facebook.com", null);
  console.log(og_html);
  // get og tags in header
  // organize into description, image, etc
}

openGraphData()

// oops! can't request open graph protocol information from arbitrary servers...
