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
 * inject a hot reload script into the body iof an html string.
 */
const injectHotReload = ({
  htmlString,
  devWebsocketUrl,
}: {
  htmlString: string;
  devWebsocketUrl: string;
}) => {
  const wsUrl = devWebsocketUrl;
  const script = `
    <script>
      console.log('hot reload script loaded');
      const socket = new WebSocket('${wsUrl}');
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
  {
    siteName,
    sourceDir,
    rootUrl,
    devWebsocketUrl,
    resourcesDir,
    faviconsDir,
    targetDir,
  }: PageSettings & { devWebsocketUrl: string }
) => {
  const { contents, mimeType } = file.serve({
    siteName,
    rootUrl,
    sourceDir,
    targetDir,
    resourcesDir,
    faviconsDir,
  });

  let responseText =
    mimeType === "text/html"
      ? injectHotReload({ htmlString: contents, devWebsocketUrl })
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
