import File from "../file/classes/file";
import type { PageSettings } from "../types/site";

/**
 * Format a URL with the URL, port, and path.
 */
const formatUrl = ({
  url,
  port,
  path,
}: {
  url: string;
  port: number;
  path?: string;
}) => `${url}${port ? ":" + port : ""}${path ?? ""}`;

/**
 * Remove the full path from the URL.
 */
const withoutUrl = (fullPath: string, url: string) => fullPath.replace(url, "");

/**
 * inject a hot reload script into the body of an html string.
 */
const injectHotReload = ({
  htmlString,
  websocketPath,
}: {
  htmlString: string;
  websocketPath: string;
}) => {
  const script = `
    <script>
      console.log('hot reload script loaded');
      const socket = new WebSocket('${websocketPath}');
      socket.addEventListener('message', function (event) {
        console.log('Received message from server ', event.data);
        window.location.reload();
      });
    </script>
  `;

  return htmlString.replace("</body>", `${script}</body>`);
};

/**
 * make a response to a request for a file with the file
 */
const makeFileResponse = (
  file: File,
  cfg: PageSettings & { websocketPath: string }
) => {
  const { contents, mimeType } = file.serve(cfg);

  let responseText =
    mimeType === "text/html"
      ? injectHotReload({
          htmlString: contents,
          websocketPath: cfg.websocketPath,
        })
      : contents;

  return new Response(responseText, {
    headers: {
      // NEVER cache. this is always a dev server.
      "Cache-Control": "no-store, no-cache, must-revalidate, proxy-revalidate",
      Pragma: "no-cache",
      Expires: "0",
      // content-type (required)
      "content-type": mimeType,
    },
  });
};

export { withoutUrl, formatUrl, makeFileResponse };
