clean: FORCE
	./gradlew clean

jar: FORCE
	./gradlew clean
	./gradlew jar

test: FORCE
	./gradlew test

eval: FORCE
	java -jar compiler.jar eval::bottomupnaive -OUT ./out -FACTS ./facts $(EVAL)

FORCE: ;
