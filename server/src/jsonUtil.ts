// @ts-check

export function parseJson(data: string): any | null {
	try {
		let res = JSON.parse(data)
		return res
	} catch (error) {
		// pass
	}
	let len = data.length
	let open = 0
	let start = 0
	for (let i = 0; i < len; i++) {
		if (data[i] == '{' && (i == 0 || data[i - 1] != "\\")) {
			if (open == 0)
				start = i
			open++;
		} else if (data[i] == '}' && open > 0 && data[i - 1] != "\\") {
			open--;
			if (open == 0) {
				try {
					return JSON.parse(data.substring(start, i + 1))
				}
				catch {
					break
				}
			}
		}
	}
	return JSON.parse(data)
}