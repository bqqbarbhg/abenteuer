
let endpoint = '';
const elOutput = document.getElementById('output');
const elInput = document.getElementById('input');

let prevMessages = [];
let currentMessages = [];
let currentCommand = null;
let prevEphemeral = false;

const date = new Date()
const session = date.getTime().toString() + Math.random().toString().substring(2);

function interact(input) {
	if (input.startsWith('/connect')) {
		const port = input.split(' ')[1];
		return connect(port);
	}
	
	const headers = new Headers();
	headers.append("X-Session", session);
	return fetch(endpoint, {
		method: "POST",
		body: input,
		headers: headers,
	}).then(response => response.json())
}

function appendSpacer() {
	const span = document.createElement('span');
	span.classList.add('out');
	span.classList.add('out-spacer');
	span.innerText = '\n';
	elOutput.appendChild(span);
	currentMessages.push(span);
}

function appendCommand(cmd) {
	const span = document.createElement('span');
	span.classList.add('out');
	span.classList.add('out-command');
	span.innerText = "\n> " + cmd + "\n";
	elOutput.appendChild(span);
	appendSpacer();
	elOutput.scrollTop = elOutput.scrollHeight;
	currentMessages.push(span);
	currentCommand = span;
}

function appendOutput(result) {

	for (part of result.parts) {
		if (prevEphemeral) {
			for (msg of prevMessages)
				elOutput.removeChild(msg);
		}

		if (part.overridePrompt != null && currentCommand) {
			currentCommand.innerText = "\n> " + part.overridePrompt + "\n";
		}

		prevEphemeral = part.ephemeral;

		for (textSpan of part.spans) {
			const span = document.createElement('span');
			span.classList.add('out');
			if (textSpan.style.bold) span.classList.add('out-bold');
			span.innerText = textSpan.text;
			elOutput.appendChild(span);
			elOutput.scrollTop = elOutput.scrollHeight;
			currentMessages.push(span);
		}

		prevMessages = currentMessages;
		currentMessages = [];
		appendSpacer();
	}
}

function connect(port) {
	endpoint = 'http://localhost:' + port;

	interact("/title").then((response) => {
		document.title = response.parts[0].spans[0].text;
	}).catch(() => {
		appendOutput({
			parts: [
				{
					spans: [
						{ text: "ERROR: ", style: { bold: true } },
						{ text: "Failed to connect to the server: ", style: { bold: false } },
						{ text: endpoint, style: { bold: true } },
						{ text: "\n\nThe server needs to be started first, run ", style: { bold: false } },
						{ text: "ui.GameServer locally. This version only tries to ", style: { bold: false } },
						{ text: "connect to a local server.", style: { bold: false } },
						{ text: "\n\nRefresh the page or type /connect (port) to retry!", style: { bold: false } },
					],
					ephemeral: false,
					overridePrompt: null,
				}
			]
		});
	});

	return interact("/hello");
};

elInput.addEventListener('keydown', (e) => {
	if (e.keyCode == 13) {
		appendCommand(elInput.value);
		interact(elInput.value).then(appendOutput);
		elInput.value = '';
		e.preventDefault();
	}
});

connect('8080').then(appendOutput);

