all: jar runner

playground.jar:
	mvn package
	find target -name "*jar-with-dependencies.jar" -exec cp {} ./playground.jar \;

jar: playground.jar

runner:
	cat src/main/resources/stub.sh playground.jar > playground
	chmod +x playground

debug: jar
	scala -cp playground.jar

clean:
	mvn clean
	rm -fr target *jar playground
