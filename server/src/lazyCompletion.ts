import { TextDocument, CompletionItem } from 'vscode-languageserver';

export function lazyCompletion(doc: TextDocument, lazyCompletions: Map<string /*uri*/, CompletionItem[]>) {
	const text = doc.getText();
	const tokens = new Set<string>();
	let last: string;
	const tokenEreg = /\w+/g;
	let m: RegExpExecArray;
	// tslint:disable-next-line: no-constant-condition
	do {
		m = tokenEreg.exec(text)
		if (!m)
			break
		const t = m.toString()
		tokens.add(last = t)
	} while (true)
	if (last != null)
		tokens.delete(last);
	const list = lazyCompletions.has(doc.uri) ? lazyCompletions.get(doc.uri) : [];
	lazyCompletions.set(doc.uri, list);
	list.splice(0, list.length);
	tokens.forEach(it => list.push(CompletionItem.create(it)));
}
