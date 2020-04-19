"use strict";

const utf8_encoder = new TextEncoder();
const utf8_decoder = new TextDecoder("utf-8");

window.onload = function () {
  const result_template = <HTMLTemplateElement>document.getElementById("result-template");
  const results = document.getElementById("results");
  const search_field = <HTMLInputElement>document.getElementById("search-field");
  search_field.onkeydown =
  event => {
    if(event.key === "Enter") {
      results.innerHTML = "";
      const resource = "/rg/" + encodeURIComponent(search_field.value);
      fetch(resource).then(
        response => { return response.text(); }
      ).then(
        s => {
          const lines = s.split("\n").filter(
            line => { return line.length != 0; }
          );
          lines.forEach(
            line => {
              const j = JSON.parse(line);
              const result = <Element>result_template.content.cloneNode(true);
              if (j.type === "match") {
                result.querySelector("h3").textContent = j.data.path.text;
                const rawstr = utf8_encoder.encode(j.data.lines.text);
                let lastpos = 0;
                let fmtstr = result.querySelector("code");
                j.data.submatches.forEach(
                  sub => {
                    fmtstr.appendChild(
                      document.createTextNode(utf8_decoder.decode(rawstr.slice(lastpos, sub.start)))
                    );
                    let fmtspan = document.createElement("span");
                    fmtspan.classList.add("match");
                    fmtspan.textContent = utf8_decoder.decode(rawstr.slice(sub.start, sub.end));
                    fmtstr.appendChild(fmtspan);
                    lastpos = sub.end;
                  }
                );
                fmtstr.appendChild(
                  document.createTextNode(utf8_decoder.decode(rawstr.slice(lastpos)))
                );
                results.appendChild(result);
              }
            }
          );
        }
      );
    }
  }
}
