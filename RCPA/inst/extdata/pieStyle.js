vizmap = [
    {
        selector: "node",
        css: {
            label: 'data(label)',
            width: 'data(size)',
            height: 'data(size)',
            "background-color": "#ccc",
            "text-valign": "bottom",
            "text-halign": "center",
            "text-margin-y": "3px",
            "color": "#000",
            "font-size": "12px",
            "border-width": 'data(borderWidth)',
            'border-color': 'data(borderColor)',
            'pie-size': '100%'
        }
    },
    {
        selector: 'edge',
        css: {
            "line-color": "#dddddd",
            "target-arrow-shape": "none",
            "target-arrow-color": "rgb(0, 0, 0)",
            "width": 'data(weight)',
            'curve-style': 'bezier'
        }
    }
];

nResult = cy.nodes()[0].data("nResult");
isDiscrete = cy.nodes()[0].data("mode") == "discrete"

pie = {};
resultNames = []

for (let i = 1; i <= nResult; ++i) {
    pie[`pie-${i}-background-color`] = (node) => node.data("color" + i);
    pie[`pie-${i}-background-size`] = 100 / nResult
    resultNames.push(cy.nodes()[0].data("resultName" + i));
}

vizmap[0].css = Object.assign(vizmap[0].css, pie);

cyDiv = document.querySelector("#cyDiv")

// Put the legend in the bottom right corner

legendDiv = document.createElement("div");
legendDiv.style.position = "absolute";
legendDiv.style.bottom = "10px";
legendDiv.style.right = "10px";
legendDiv.style.width = "200px";
legendDiv.style.height = "auto";
legendDiv.style.border = "1px solid black";
legendDiv.style.borderRadius = "4px";
legendDiv.style.padding = "8px";
legendDiv.style.overflow = "auto";

transforms = []
for (let i = 0; i < nResult; ++i) {
    transforms.push(`rotate(${i * 360 / nResult}deg)`)
}

legendDiv.innerHTML = `
<div>
    <div style="height: 100px; width: 100px; overflow: hidden; border: 1px solid #777; position: absolute; top: 12px; left: 50%; transform: translate(-50%, 0); border-radius: 50%;">
    ${
    transforms.map((e, i) => `
        <div style="position: absolute; transform-origin: bottom left; bottom: 50%; left: 50%; width: 0px; height: 52px; border: solid 1px #777; transform: rotate(${i * 360 / nResult}deg);"></div>
        <div style="position: absolute; transform-origin: bottom left; bottom: 50%; left: 50%; height: 50px; transform: rotate(${i * 360 / nResult + 360 / nResult / 2}deg);">
            <div style="margin-top: 5px">
                <div style="transform: rotate(-${i * 360 / nResult + 360 / nResult / 2}deg)">
                    ${i + 1}
                </div>
            </div>
        </div>
        `).join("")
}
    </div>
    
    <div style="margin-top: 110px">
        ${resultNames.map((name, i) => `<div>${i + 1}. ${name}</div>`).join("")}
    </div>
</div>
`

cyDiv.appendChild(legendDiv);

legendDiv = document.createElement("div");

legendDiv.style.position = "absolute";
legendDiv.style.bottom = "5px";
legendDiv.style.right = "210px";
legendDiv.style.width = "200px";
legendDiv.style.height = "50px";

legendTitle = cy.nodes()[0].data("legendTitle");
statLimitUpper = cy.nodes()[0].data("statLimitUpper");
statLimitLower = cy.nodes()[0].data("statLimitLower");

legendDiv.innerHTML = isDiscrete ? `` : `
<div>
    <div style=" margin-left: 15px; margin-bottom: 5px">
        ${legendTitle}
    </div>
    <div style="width: 175px;">
        <div style="float: left">
            ${statLimitLower}
        </div>
        <div style="float: right">
            ${statLimitUpper}
        </div>
    </div>
    ${
    statLimitLower < 0 ?
        `
        <div style="
            margin-left: 15px;
            background: linear-gradient(90deg, rgba(0,0,255,1) 0%, rgba(255,255,255,1) 50%, rgba(255,0,0,1) 100%);
            width: 150px;
            height: 20px;
        "></div>
        `
        :
        `
        <div style="
            margin-left: 15px;
            background: linear-gradient(90deg, rgba(255,255,255,1) 0%, rgba(255,0,0,1) 100%);
            width: 150px;
            height: 20px;
        "></div>
        `
}
    
</div>
`

cy.style(vizmap);
cyDiv.appendChild(legendDiv);
