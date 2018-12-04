clean: FORCE
	./gradlew clean

jar: FORCE
	./gradlew clean
	./gradlew jar

test: FORCE
	./gradlew test

eval: FORCE
	java -jar compiler.jar eval::bottomupnaive -OUT ./out -FACTS ./facts $(EVAL)

prettyinternal: FORCE
	java -jar compiler.jar pretty::bottomupnaive -OUT ./out -FACTS ./facts $(EVAL)

prettysouffle: FORCE
	java -jar compiler.jar pretty::souffle -OUT ./out -FACTS ./facts $(EVAL)

typeprog: FORCE
	java -jar compiler.jar eval::typeprog $(EVAL)

FORCE: ;
