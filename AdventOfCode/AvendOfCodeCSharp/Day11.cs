using System;
using System.Collections.Generic;
using System.Linq;

namespace AvendOfCodeCSharp
{
    class Day11
    {
        public void SolvePart1()
        {
            var data = GetInput(9810, 300, 300);
            var result = CalculateScores(data, 3).OrderByDescending(x => x.score).First();
            Console.WriteLine(result);
        }

        public void SolvePart2()
        {
            var data = GetInput(9810, 300, 300);
            (int x, int y, int score, int squareSize) result = (0, 0, 0, 0);
            for (var i = 1; i <= 300; i++)
            {
                var next = CalculateScores(data, i).OrderByDescending(x => x.score).First();
                if (next.score >= result.score)
                    result = next;

                if (next.score < 0)
                    break;
            }
            Console.WriteLine(result);
        }

        private int[,] GetInput(int serialNumber, int width, int heigth)
        {
            var result = new int[width, heigth];
            for(var x = 0; x < width; x++)
            for (var y = 0; y < heigth; y++)
                result[x, y] = CalculatePowerlevel(x, y, serialNumber);

            return result;
        }

        private int CalculatePowerlevel(int x, int y, int serialNumber)
        {
            var rackId = x + 10;
            var powerlevel = (((((rackId * y) + serialNumber) * rackId) / 100) % 10) - 5;

            if (powerlevel > 9000)
            {
                Console.WriteLine("Vegeta, what does the scouter say about his power?");
                Console.WriteLine("It's over NINE THOUSAAAAAAAND!");
                Console.WriteLine("WHAT?! NINE THOUSAND?!");
            }

            return powerlevel;
        }

        private IEnumerable<(int x, int y, int score, int squareSize)> CalculateScores(int[,] field, int squareSize)
        {
            for(var x = 0; x < field.GetLength(0) - squareSize; x++)
            for (var y = 0; y < field.GetLength(1) - squareSize; y++)
            {
                var score = 0;
                for(var runx = 0; runx < squareSize; runx++)
                for (var runy = 0; runy < squareSize; runy++)
                    score += field[x + runx, y + runy];
                yield return (x, y, score, squareSize);
            }
        }
    }
}
