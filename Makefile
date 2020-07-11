help:
	@echo "make -s init - initialize project"
	@echo "make -s test - run unit tests"
	@echo "make -s run - run the main script"
	@echo "make -s version-set - display the current artifact version"
	@echo "make -s version-get VERSION=<new version> - Update version number to a new one."
	@echo "make -s clean - delete generated files"
	@echo "make -s verify - run tests and check javadocs"
	@echo "make -s release - release to maven central"
	@echo "-s option hides the Make invocation command (@echo)."

init:
	@echo init

test: pmd-dev
	mvn test

pmd-dev:
	sed -i '' 's/PMD-roycetech-2020-single-thread-test/PMD-roycetech-2020-single-thread-main/g' .eclipse-pmd

pmd-test:
	sed -i '' 's/PMD-roycetech-2020-single-thread-main/PMD-roycetech-2020-single-thread-test/g' .eclipse-pmd

run:
	@echo run

version-get:
	@mvn org.apache.maven.plugins:maven-help-plugin:2.1.1:evaluate \
		-Dexpression=project.version|grep -Ev '(^\[|Download\w+:)'

version-set:
	mvn versions:set -DnewVersion=${VERSION}

verify:
	mvn verify -Dgpg.skip

release:
	mvn clean deploy -P release

clean:
	rm -rf target/
	rm -rf bin/

.PHONY: help test init clean run version-set version-get release verify pmd-dev pmd-test
