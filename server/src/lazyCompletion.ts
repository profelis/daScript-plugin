import { TextDocument, CompletionItem } from 'vscode-languageserver';

export function lazyCompletion(doc: TextDocument, lazyCompletions: Map<string /*uri*/, CompletionItem[]>) {
	let text = doc.getText();
	let tokens = new Set<string>();
	let last: string;
	var tokenEreg = /\w+/g;
	var m: RegExpExecArray;
	do {
		m = tokenEreg.exec(text);
		if (!m)
			break;
		let t = m.toString();
		tokens.add(last = t);
	} while (true);
	if (last != null)
		tokens.delete(last);
	let list = lazyCompletions.has(doc.uri) ? lazyCompletions.get(doc.uri) : [];
	lazyCompletions.set(doc.uri, list);
	list.splice(0, list.length);
	tokens.forEach(it => list.push(CompletionItem.create(it)));
}
