#include <fstream>
#include <iostream>
#include <math.h>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <assert.h>
#include <random>
#include <chrono>
#include "debug.h"

#ifndef TTPinstance
#define TTPinstance

class TTPSolution {
public:
  TTPSolution(std::vector<int> tour, std::vector<int> packing) {
    this->tour = tour;
    this->packing = packing;
    this->value = INFINITY;
  }

  void setValue(double value) {
    this->value = value;
  }

  double getValue() const {
    return(this->value);
  }

  std::vector<int> getTour() {
    return(this->tour);
  }

  std::vector<int> getPacking() {
    return(this->packing);
  }

  // Overloading < operator
  bool operator <(const TTPSolution &solution) {
    if (value < solution.value) {
      return(true);
    }
    return(false);
  }

  void print() {
    printf("Solution value: %.3f", this->value);
  }

private:
  double value;
  std::vector<int> tour;
  std::vector<int> packing;
};

class TTPAlgorithmOutput {
public:
  TTPAlgorithmOutput(TTPSolution solution, std::vector<double> trajectory) : solution(solution) {
    this->trajectory = trajectory;
  }

  void setFinalTours(std::vector<std::vector<int>> finalTours) {
    this->finalTours = finalTours;
  }

  TTPSolution getSolution() {
    return(this->solution);
  }

  std::vector<double> getTrajectory() {
    return(this->trajectory);
  }

  std::vector<std::vector<int>> getFinalTours() {
    return(this->finalTours);
  }

private:
  TTPSolution solution;
  std::vector<double> trajectory;
  std::vector<std::vector<int>> finalTours;
};

class TTPInstance {
public:
  TTPInstance(std::string pathToFile) {
    std::ifstream infile(pathToFile, std::ios::in);

    std::string line;
    std::size_t pos;
    std::string prefix;

    do {
      std::getline(infile, line);
      prefix = "PROBLEM NAME";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->problemName = line.substr(pos + 1);
      }
      prefix = "KNAPSACK DATA TYPE";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->knapsackDataType = line.substr(pos + 1);
      }
      prefix = "DIMENSION";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->numberOfNodes = stoi(line.substr(pos + 1));
      }
      prefix = "NUMBER OF ITEMS";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->numberOfItems = stoi(line.substr(pos + 1));
      }
      prefix = "CAPACITY OF KNAPSACK";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->capacity = stoi(line.substr(pos + 1));
      }
      prefix = "MIN SPEED";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->minSpeed = stod(line.substr(pos + 1));
      }
      prefix = "MAX SPEED";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->maxSpeed = stod(line.substr(pos + 1));
      }
      prefix = "RENTING RATIO";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->rentingRatio = stod(line.substr(pos + 1));
      }
      //FIXME: two times renting ratio?
      prefix = "RENTING RATIO";
      if (line.substr(0, prefix.length()) == prefix) {
        pos = line.find(":");
        this->edgeWeightType = line.substr(pos + 1);
      }

      // stop once nodes found
      prefix = "NODE_COORD_SECTION";
      if (line.substr(0, prefix.length()) == prefix) {
        break;
      }
    } while(true);


    // Now read the nodes
    int index, xpos, ypos;

    int i = 1;

    while (i <= this->numberOfNodes) {
      i += 1;
      infile >> index >> xpos >> ypos;
      std::vector<int> node {index, xpos, ypos};
      this->nodes.push_back(node);
    }

    // read line starting with "ITEMS SECTION"
    std::getline(infile, line);
    std::getline(infile, line);

    // Now for the items
    int itemIndex, profit, weight, nodeNumber;
    i = 1;
    while (i <= this->numberOfItems) {
      i += 1;
      infile >> itemIndex >> profit >> weight >> nodeNumber;
      std::vector<int> item {itemIndex, profit, weight, nodeNumber};
      this->items.push_back(item);
    }

    infile.close();

    //this->print(true);
  }

  void print(bool detailed = false) {
    printf("TTP Problem: %s\n", this->problemName.c_str());
    printf("(nodes: %i, items: %i, capacity: %i)\n", this->numberOfNodes, this->numberOfItems, this->capacity);
    if (detailed) {
      // head of nodes
      //int toShow = std::min(10, this->numberOfNodes);
      int toShow = 10;
      //toShow = this->numberOfNodes;
      printf("\nNODES (first %i)\n", toShow);
      for (int i = 0; i < toShow; ++i) {
        std::vector<int> node = this->nodes[i];
        printf("(index=%d, x=%d, y=%d)\n", node[0], node[1], node[2]);
      }

      //toShow = std::min(10, this->numberOfItems);
      printf("\nITEMS (first %i)\n", toShow);
      //toShow = this->numberOfItems;
      for (int i = 0; i < toShow; ++i) {
        std::vector<int> item = this->items[i];
        printf("(index=%d, p=%d, w=%d, node=%d)\n", item[0], item[1], item[2], item[3]);
      }
    } // if (detailed)
  }

  double evaluate(TTPSolution solution, double gamma) {
    // tour not neccessarily starting with 1 (will be rotated later)
    std::vector<int> tour = solution.getTour();

    // at position i this is already the sum of items weights of node i
    std::vector<int> packing = solution.getPacking();

    double length = 0;
    double weightSum = 0;
    unsigned int n = this->numberOfNodes;

    // first node needs to be 1. Find 1
    std::vector<int> rotatedTour(n);
    int cityOneIdx = 0;
    for (int i = 0; i < n; ++i) {
      if (tour[i] == 1) {
        cityOneIdx = i;
        break;
      }
    }

    // now rotate
    for (int i = 0; i < n; ++i) {
      int idxInTour = (i + cityOneIdx) % n;
      rotatedTour[i] = tour[idxInTour];
    }

    // now calculate tour
    for (int i = 0; i < n - 1; ++i) {
      weightSum += packing[rotatedTour[i] - 1];
      length += weightSum * this->getDistance(rotatedTour[i], rotatedTour[i + 1]);
    }
    weightSum += packing[rotatedTour[n - 1] - 1];
    length += weightSum * this->getDistance(rotatedTour[n - 1], rotatedTour[0]);

    solution.setValue(length);
    return(length);
  }

  int getNumberOfNodes() {
    return(this->numberOfNodes);
  }

  int getNumberOfItems() {
    return(this->numberOfItems);
  }

  std::string getName() {
    return(this->problemName);
  }

  std::vector<std::vector<int>> getItems() {
    return(this->items);
  }

  double getDistance(int i, int j) {
    std::vector<int> x = this->nodes[i - 1];
    std::vector<int> y = this->nodes[j - 1];
    double x1 = x[1], x2= x[2], y1 = y[1], y2 = y[2];
    double d = sqrt((x1 - y1) * (x1 - y1) + (x2 - y2) * (x2 - y2));
    return(d);
  }

  // void precalculatePacking(const double p, const int times, std::string filename) {
  //   // Bernoulli sampler
  //   std::default_random_engine generator;
  //   std::bernoulli_distribution bernoulliExperiment(p);

  //   // output file
  //   std::ofstream output;
  //   output.open(filename, std::ios::out);

  //   // generate packings and store to file
  //   for (int i = 0; i < times; ++i) {
  //     for (int j = 0; j < this->numberOfItems; ++j) {
  //       output << bernoulliExperiment(generator);
  //       if (j < this->numberOfItems - 1) {
  //         output << " ";
  //       } else {
  //         output << "\n";
  //       }
  //     }
  //   }

  //   // cleanup
  //   output.close();
  // }

private:
  std::string problemName;
  std::string knapsackDataType;
  std::string edgeWeightType;
  unsigned int numberOfNodes;
  unsigned int numberOfItems;
  unsigned int capacity;
  double minSpeed;
  double maxSpeed;
  double rentingRatio;
  std::vector<std::vector<int>> nodes;
  std::vector<std::vector<int>> items;
};

class TTPAlgorithm {
public:
  static TTPAlgorithmOutput doEAMagic(
    TTPInstance instance,
    std::vector<int> packing,
    std::vector<std::vector<int>> tours,
    int mu,
    int lambda,
    int mutation,
    int survivalStrategy, // 0 = best of (mu + 1), 1 = keep best of parent and child
    int maxEvaluations,
    double gamma
    ) {

    printf("Running (%d + %d)-EA on instance %s\n", mu, lambda, instance.getName().c_str());

    // extract some vars
    int n = instance.getNumberOfNodes();
    std::vector<std::vector<int>> items = instance.getItems();

    // Calculate weight per node
    std::vector<int> packingByNode(n);
    for (int i = 0; i < packingByNode.size(); ++i) {
      packingByNode[i] = 0;
    }
    // first item has weight 1
    packingByNode[0] = 1;
    for (int i = 0; i < packing.size(); ++i) {
      if (packing[i] == 1) {
        packingByNode[items[i][3] - 1] += items[i][2];
      }
    }

    // init random number generator
    std::default_random_engine rndgenerator;
    //FIXME: check if possible if mu = 1. Then we sample from [0,0]
    std::uniform_int_distribution<int> Unif(0, mu - 1);

    std::vector<TTPSolution> population;
    for (int i = 0; i < mu; ++i) {
      TTPSolution individual(tours[i], packingByNode);
      double tourLength = instance.evaluate(individual, gamma);
      individual.setValue(tourLength);
      population.push_back(individual);
    }

    // init stopping condition counters
    unsigned int iteration = 0;
    unsigned int evaluations = mu;

    std::vector<double> trajectory(maxEvaluations);

    while (evaluations < maxEvaluations) {
      // sample random individual from population
      int parentId = Unif(rndgenerator);
      std::vector<int> child = population[parentId].getTour();

      // now perform mutation
      if (mutation == 0) {
        // SWAP mutation: select two genes at random and swap their
        // positions
        int positionA = rand() % n;
        int positionB = rand() % n;
        // now swap
        int tmp = child[positionA];
        child[positionA] = child[positionB];
        child[positionB] = tmp;
      } else if (mutation == 1) {
        // JUMP mutation:
        int positionA = rand() % n;
        int positionB = rand() % n;
        if (positionA != positionB) {
          // assure that A is lower than B
          if (positionB < positionA) {
            int tmp = positionA;
            positionA = positionB;
            positionB = tmp;
          }
          int tmp = child[positionB];
          for (int i = positionB - 1; i >= positionA; --i) {
            child[i + 1] = child[i];
          }
          child[positionA] = tmp;
        }
      } else if (mutation == 2) {
        // INVERSION mutation: select two positions at random and
        // reverse order between those partitions
        int positionA = rand() % n;
        int positionB = rand() % n;
        if (positionA != positionB) {
          // assure that A is lower than B
          if (positionB < positionA) {
            int tmp = positionA;
            positionA = positionB;
            positionB = tmp;
          }
          int i = positionA;
          int j = positionB;
          while (i < j) {
            int tmp = child[i];
            child[i] = child[j];
            child[j] = tmp;
            ++i;
            --j;
          }
        }
      } else if (mutation == 3) {
        // SCRAMBLE mutation: select two positions at random
        // and scramble all elements inbetween
        int positionA = rand() % n;
        int positionB = rand() % n;
        if (positionA != positionB) {
          // assure that A is lower than B
          if (positionB < positionA) {
            int tmp = positionA;
            positionA = positionB;
            positionB = tmp;
          }
          //FIXME: should work with random_shuffle also!? However,
          // I have no clue how to pass beginning and end.
          child = scramblePartialTour(child, positionA, positionB);
        }
      }
      // else if (mutation == 4) {
      //   // 2-OPT: go through 2-neighborhood and pick best improvement
      //   bool improved = false;
      //   // iterate over first node
      //   for (int i = 0; i < n; ++i) {
      //     if (improved) {
      //       break;
      //     }
      //     // iterate over second node
      //     for (int j = i + 1; j < n; ++j) {
      //       if (improved) {
      //         break;
      //       }
      //       // 2-opt move
      //       child = incumbantTour;
      //       int k = i, l = j;
      //       while (k < l) {
      //         int tmp = child[k];
      //         child[k] = child[l];
      //         child[l] = tmp;
      //         ++k;
      //         --l;
      //       }
      //       // evaluate
      //       TTPSolution childSolution(child, packingByNode);
      //       double newValue = instance.evaluate(childSolution, gamma);
      //       if (newValue < incumbantValue) {
      //         improved = true;
      //       }
      //     }
      //   }
      // }

      // caluclate function value of mutant
      TTPSolution childSolution(child, packingByNode);
      double childLength = instance.evaluate(childSolution, gamma);
      childSolution.setValue(childLength);

      // decide which survival strategy to perform
      if (survivalStrategy == 0) {
        // Do classical select mu from (mu+1) strategy
        population.push_back(childSolution);

        sort(population.begin(), population.end(), [](const TTPSolution& s1, const TTPSolution& s2) {
          return s1.getValue() < s2.getValue();
        });

        // delete last, i.e. worst, element
        population.pop_back();
      } else if (survivalStrategy == 1) {
        // Compare with parent only
        double parentLength = population[parentId].getValue();
        if (childLength <= parentLength) {
          population[parentId] = childSolution;
        }

        sort(population.begin(), population.end(), [](const TTPSolution& s1, const TTPSolution& s2) {
          return s1.getValue() < s2.getValue();
        });
      }

      // log progress
      // for (mu + 1) the first lambda evaluations are simply the best out of mu
      double incumbantValue = population[0].getValue();
      if (iteration == 0) {
        for (int i = 0; i < mu; ++i) {
          trajectory[i] = incumbantValue;
        }
      } else {
        trajectory[evaluations] = incumbantValue;
      }
      trajectory[iteration] = population[0].getValue();
      if ((iteration >= 1) && (trajectory[iteration] < trajectory[iteration - 1])) {
        printf("New best value at iteration %d is %.2f\n", iteration, trajectory[iteration]);
      }

      ++iteration;
      ++evaluations;
    } // maxEvaluations

    printf("EA terminated. Wrapping up ...\n");

    std::vector<std::vector<int>> toursFinal(mu);
    for (int i = 0; i < mu; ++i) {
      toursFinal[i] = population[i].getTour();
    }

    TTPAlgorithmOutput algorithmOutput(population[0], trajectory);// toursFinal);
    algorithmOutput.setFinalTours(toursFinal);

    return(algorithmOutput);
  }

  static std::vector<int> generateRandomPermutation(int n) {
    std::vector<int> tour(n);
    // tours are permutations of {1, ..., n}
    for (int i = 0; i < n - 1; ++i) {
      tour[i] = i + 1;
    }
    std::random_shuffle(tour.begin(), tour.end());
    return(tour);
  }

  static std::vector<int> scramblePartialTour(std::vector<int> tour, int start, int end) {
    // copy relevant part
    std::vector<int> partialTour(end - start + 1);
    for (int i = start; i <= end; ++i) {
      partialTour[i - start] = tour[i];
    }
    // acutally scramble
    std::random_shuffle(partialTour.begin(), partialTour.end());
    // copy back into original
    for (int i = 0; i < partialTour.size(); ++i) {
      tour[i + start] = partialTour[i];
    }
    return(tour);
  }
};

#endif // TTPInstnace
