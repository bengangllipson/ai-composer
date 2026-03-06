# AI Composer

[![Latest Release](https://maven-badges.sml.io/sonatype-central/com.bengangllipson/ai-composer_3/badge.svg?subject=Latest%20Release&color=blue)](https://maven-badges.sml.io/sonatype-central/com.bengangllipson/ai-composer_3/)

This project explores the potential for generating music using artificial intelligence. It generates notes via the 
musical technique of counterpoint to facilitate harmonically cohesive melodies. Two methods are exposed, one for 
generating a cantus firmus line and one for generating a first species for a given cantus firmus. Both methods adhere 
to a set of rules which dictate which notes can be chosen when.

## Installation
### maven
```xml
<dependency>
    <groupId>com.bengangllipson</groupId>
    <artifactId>ai-composer_3</artifactId>
    <version>3.0.0</version>
</dependency>
```

### gradle
```groovy
implementation("com.bengangllipson:ai-composer_3:3.0.0")
```

### sbt
```scala 3
libraryDependencies += "com.bengangllipson" % "ai-composer_3" % "3.0.0"
```

## Usage
```java
import com.bengangllipson.aicomposer.api.AiComposer;

void main() {
    AiComposer aiComposer = new AiComposer();
    List<String> cantusFirmus = aiComposer.generateCantusFirmus();
    List<String> firstSpecies = aiComposer.generateFirstSpecies(cantusFirmus);
}
```

## Dependencies
- [Scala v3.8.2](https://www.scala-lang.org/download/all.html)
- [JDK 17+](https://www.oracle.com/java/technologies/downloads/)
- [sbt](https://www.scala-sbt.org/download/)

## Building
`$ sbt clean test compile publishsigned`

## Testing
The test suite includes both typical unit tests and "fuzzing" tests to test the random behavior of the algorithm. The 
number of tries directly corresponds with the level of confidence in the random behavior. To change the number of tries 
alter the `val TRIES` inside of the companion `object` for each fuzzing test class.

`$ sbt clean test`

### Cantus Firmus Rules
- Length must be 8-16 notes.
- First and last note must be the tonic.
- Final note must be approached by either a step above or a half step below.
- The leading tone must always progress to the tonic.
- All notes must be in the correct key.
- All note-to-note progressions must be melodic consonances.
- No note may be followed by the same note.
- The total range (interval between lowest and highest notes) must be no more than a tenth.
- There must be no more than 2 melodic motions larger a 4th or larger.
- Any large leaps (a 4th or larger) must be followed by stepwise motion in the opposite direction.
- There must be a single climax (high point) that appears only once in the line.
- Motives or licks must not be repeated.

### First Species Rules
- Must be the same length as the cantus firmus.
- The first note must be either the tonic or the dominant.
- The last note must be the tonic.
- All notes must be in the correct key.
- All note-to-note progressions must be melodic consonances.
- There must be no leaps greater than an 8th.
- All vertical intervals between the cantus firmus and first species must be harmonic consonances.
- No note may be followed by the same note.
- The leading tone must always lead to the tonic.
- Any large leaps (a 4th or larger) must be followed by stepwise motion in the opposite direction.
- There must be no more than 2 leaps in a row.
- Consecutive leaps must not go in the same direction.
- There must be a single climax (high point) that appears only once in the line.
- Motives or licks must not be repeated.
- The line must not contain the same note more than 4 times.
- There must be no parallel 5ths.
