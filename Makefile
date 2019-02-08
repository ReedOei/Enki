test:
	bash "tests/test.sh" "enki.maude" tests/*-test.sh
	cd Parser
	stack test

