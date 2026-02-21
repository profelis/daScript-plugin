const js = require('@eslint/js')
const tsParser = require('@typescript-eslint/parser')
const tsPlugin = require('@typescript-eslint/eslint-plugin')

module.exports = [
	js.configs.recommended,
	{
		files: ['client/src/**/*.ts', 'client/src/**/*.tsx', 'server/src/**/*.ts', 'server/src/**/*.tsx'],
		languageOptions: {
			parser: tsParser,
			ecmaVersion: 'latest',
			sourceType: 'module',
		},
		plugins: {
			'@typescript-eslint': tsPlugin,
		},
		rules: {
			semi: ['error', 'never'],
			'no-unused-vars': 'off',
			'no-undef': 'off',
			'no-unreachable': 'off',
			'no-useless-assignment': 'off',
			'@typescript-eslint/no-unused-vars': 0,
			'@typescript-eslint/no-explicit-any': 0,
			'@typescript-eslint/explicit-module-boundary-types': 0,
			'@typescript-eslint/no-non-null-assertion': 0,
		},
	},
]
