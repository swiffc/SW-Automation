using SolidWorks.Interop.sldworks;
using System;
using System.Diagnostics;
using System.Numerics;
using System.Reflection;
using static Hood.Trig;
using mTools = Tools.ModelTools;


namespace Hood
{
    internal static class Trig 
    {
        public struct Point
        {
            public double x, y, z;

            public Point(double x, double y, double z)
            {
                this.x = x;
                this.y = y;
                this.z = z;
            }

            // Method to add two points
            public static Point operator +(Point a, Point b)
            {
                return new Point(a.x + b.x, a.y + b.y, a.z + b.z);
            }

            // Method to subtract two points
            public static Point operator -(Point a, Point b)
            {
                return new Point(a.x - b.x, a.y - b.y, a.z - b.z);
            }

            // Method to scale a point
            public static Point operator *(double scalar, Point p)
            {
                return new Point(p.x * scalar, p.y * scalar, p.z * scalar);
            }

            // Method to get the length of the vector
            public double Length()
            {
                return Math.Sqrt(x * x + y * y + z * z);
            }

            // Method to normalize the vector
            public Point Normalize()
            {
                double length = Length();
                return new Point(x / length, y / length, z / length);
            }
        }
        public struct Plane
        {
            public double a, b, c, d; // ax + by + cz + d = 0
            public string Name;
        }
        public static Plane GetPlaneFromPoints(Point p1, Point p2, Point p3, string planeName)
        {
            // Calculate the vectors formed by the given points
            Point v1 = new Point(p2.x - p1.x, p2.y - p1.y, p2.z - p1.z);
            Point v2 = new Point(p3.x - p1.x, p3.y - p1.y, p3.z - p1.z);

            // Cross product of vectors to get plane normal
            Point normal = new Point(
                v1.y * v2.z - v1.z * v2.y,
                v1.z * v2.x - v1.x * v2.z,
                v1.x * v2.y - v1.y * v2.x
            );

            // Normalize the normal
            double magnitude = Math.Sqrt(normal.x * normal.x + normal.y * normal.y + normal.z * normal.z);
            normal.x /= magnitude;
            normal.y /= magnitude;
            normal.z /= magnitude;

            // Plane equation is ax + by + cz + d = 0
            // Using the point p1, we can find d
            double d = -(normal.x * p1.x + normal.y * p1.y + normal.z * p1.z);

            Plane plane = new Plane
            {
                a = normal.x,
                b = normal.y,
                c = normal.z,
                d = d,
                Name = planeName
            };


            //-----------------------------------------------------------//
            //---------------------Solidworks----------------------------//



            //-----------------------------------------------------------//
            //---------------------Debug log-----------------------------//

            Debug.WriteLine
            (
                planeName + "\n" +
                "Plane comprised of points:" + "\n" +
                $"   ({p1.x}, {p1.y}, {p1.z})" + "\n" +
                $"   ({p2.x}, {p2.y}, {p2.z})" + "\n" +
                $"   ({p3.x}, {p3.y}, {p3.z})" + "\n" +
                $"Equation: {plane.a}x + {plane.b}y + {plane.c}z + {plane.d} = 0" + "\n"
            );

            //-----------------------------------------------------------//
            //-----------------------------------------------------------//

            return plane;
        }
        public static Plane GetMidPlane(Plane plane1, Plane plane2, Point point1, Point point2)
        {
            // Calculate the mid-normal between the two plane normals
            Point midNormal = new Point(
                (plane1.a + plane2.a) / 2,
                (plane1.b + plane2.b) / 2,
                (plane1.c + plane2.c) / 2
            );

            // Normalize the midNormal to avoid potential scaling issues
            double magnitude = Math.Sqrt(midNormal.x * midNormal.x + midNormal.y * midNormal.y + midNormal.z * midNormal.z);
            midNormal.x /= magnitude;
            midNormal.y /= magnitude;
            midNormal.z /= magnitude;

            // Calculate a midpoint between the two given points
            Point midPoint = new Point(
                (point1.x + point2.x) / 2,
                (point1.y + point2.y) / 2,
                (point1.z + point2.z) / 2
            );

            // Calculate d for the mid-plane using the mid-normal and the midpoint
            double d = -(midNormal.x * midPoint.x + midNormal.y * midPoint.y + midNormal.z * midPoint.z);

            Plane plane = new Plane
            {
                a = midNormal.x,
                b = midNormal.y,
                c = midNormal.z,
                d = d,
                Name = $"MidPlane[{plane1.Name}, {plane2.Name}]"
            
            };

            //-----------------------------------------------------------//
            //---------------------Solidworks----------------------------//

            

            //-----------------------------------------------------------//
            //---------------------Debug log-----------------------------//

            Debug.WriteLine
            (
                plane.Name + "\n" +
                $"   ({point1.x}, {point1.y}, {point1.z})" + "\n" +
                $"   ({point2.x}, {point2.y}, {point2.z})" + "\n" +
                $"Equation: {plane.a}x + {plane.b}y + {plane.c}z + {plane.d} = 0" + "\n"
            );

            //-----------------------------------------------------------//
            //-----------------------------------------------------------//

            return plane;
        }
        public static Point IntersectionOfLineWithPlane(Point point2, Point point4, Plane plane, Point point1, Point point3)
        {
            // Direction vector of the line
            Point direction = new Point(point4.x - point2.x, point4.y - point2.y, point4.z - point2.z);

            // Compute the dot product of the direction vector and the plane's normal
            double denominator = plane.a * direction.x + plane.b * direction.y + plane.c * direction.z;

            // Compute t
            double t = -(plane.a * point2.x + plane.b * point2.y + plane.c * point2.z + plane.d) / denominator;

            // Compute the intersection point using the line equation
            Point intersection =  new Point(
                point2.x + t * direction.x,
                point2.y + t * direction.y,
                point2.z + t * direction.z
            );

            

            //-----------------------------------------------------------//
            //---------------------Solidworks----------------------------//



            //-----------------------------------------------------------//
            //---------------------Debug log-----------------------------//

            Debug.WriteLine
            (
                $"Rectangle bounded by points:" + "\n" +
                $"   ({point1.x}, {point1.y}, {point1.z})" + "\n" +
                $"   ({point2.x}, {point2.y}, {point2.z})" + "\n" +
                $"   ({point3.x}, {point3.y}, {point3.z})" + "\n" +
                $"   ({point4.x}, {point4.y}, {point4.z})" + "\n" +
                $"      Intersects at:" + "\n" +
                $"         ({point1.x}, {point1.y}, {point1.z})" + "\n" +
                $"         ({intersection.x}, {intersection.y}, {intersection.z})" + "\n"
            );

            //-----------------------------------------------------------//
            //-----------------------------------------------------------//

            return intersection;
        }
        public static Plane GetPerpendicularPlane(Point point1, Point point2, Plane plane)
        {
            // Extracting the normal of the midPlane
            Point midPlaneNormal = new Point(plane.a, plane.b, plane.c);

            // Direction vector of the line joining p1 and p2
            Point lineDirection = new Point(point2.x - point1.x, point2.y - point1.y, point2.z - point1.z);

            // Cross product of vectors to get the normal of the new plane
            Point newPlaneNormal = new Point(
                lineDirection.y * midPlaneNormal.z - lineDirection.z * midPlaneNormal.y,
                lineDirection.z * midPlaneNormal.x - lineDirection.x * midPlaneNormal.z,
                lineDirection.x * midPlaneNormal.y - lineDirection.y * midPlaneNormal.x
            );

            // Normalize the newPlaneNormal
            double magnitude = Math.Sqrt(newPlaneNormal.x * newPlaneNormal.x + newPlaneNormal.y * newPlaneNormal.y + newPlaneNormal.z * newPlaneNormal.z);
            newPlaneNormal.x /= magnitude;
            newPlaneNormal.y /= magnitude;
            newPlaneNormal.z /= magnitude;

            // Calculate D using the point p1
            double d = -(newPlaneNormal.x * point1.x + newPlaneNormal.y * point1.y + newPlaneNormal.z * point1.z);

            Plane perpendicularPlane =  new Plane { a = newPlaneNormal.x, b = newPlaneNormal.y, c = newPlaneNormal.z, d = d };

            Debug.WriteLine
            (
                "MidPlane rotated 90 degrees by the axis formed between points:" + "\n" +
                $"   ({point1.x}, {point1.y}, {point1.z})" + "\n" +
                $"   ({point2.x}, {point2.y}, {point2.z})" + "\n" +
                $"PerpendicularPlane equation: {perpendicularPlane.a}x + {perpendicularPlane.b}y + {perpendicularPlane.c}z + {perpendicularPlane.d} = 0" + "\n"
            );

            return perpendicularPlane;
        }
        public static Plane GetPerpendicularPlane(Point topPoint, Point bottomPoint)
        {
            // Calculate the normal vector
            double nx = topPoint.x - bottomPoint.x;
            double ny = topPoint.y - bottomPoint.y;
            double nz = topPoint.z - bottomPoint.z;

            // Calculate coefficients for the plane equation
            double a = nx;
            double b = ny;
            double c = nz;
            double d = -(nx * bottomPoint.x + ny * bottomPoint.y + nz * bottomPoint.z);

            return new Plane { a = a, b = b, c = c, d = d, Name = "Perpendicular Plane" };
        }

        public static void AAS(bool useAdjacentAngle, double adjacentAngle, double oppositeSide, out double adjacentSide, out double hypotenuse)
        {
            double adjacentAngleRadians = adjacentAngle * (Math.PI / 180.0);
            hypotenuse = oppositeSide / Math.Sin(adjacentAngleRadians);
            adjacentSide = oppositeSide * Math.Tan(adjacentAngleRadians);
        }
        public static void AAS(bool useAdjacentAngle, double adjacentAngle, out double oppositeSide, double adjacentSide, out double hypotenuse)
        {
            double adjacentAngleRadians = adjacentAngle * (Math.PI / 180.0);
            hypotenuse = adjacentSide / Math.Cos(adjacentAngleRadians);
            oppositeSide = adjacentSide * Math.Tan(adjacentAngleRadians);
        }


        public static Point ProjectPointOntoPlane(Point point, Plane plane)
        {
            // Line: sourcePoint + t * planeNormal
            Point planeNormal = new Point(plane.a, plane.b, plane.c);

            // Finding t when the line intersects the plane
            double t = -(plane.a * point.x + plane.b * point.y + plane.c * point.z + plane.d) /
                       (plane.a * planeNormal.x + plane.b * planeNormal.y + plane.c * planeNormal.z);

            // Calculating the intersection point which is the projected point
            Point projectedPoint = new Point(
                point.x + t * planeNormal.x,
                point.y + t * planeNormal.y,
                point.z + t * planeNormal.z
            );

            //-----------------------------------------------------------//
            //---------------------Solidworks----------------------------//

            

            //-----------------------------------------------------------//
            //---------------------Debug log-----------------------------//

            Debug.WriteLine
            (
                $"XZ plane point:" + "\n" +
                $"   ({point.x}, {point.y}, {point.z})" + "\n" +
                $"      Projected only perpendicular plane:" + "\n" +
                $"         PerpendicularPlane equation: {plane.a}x + {plane.b}y + {plane.c}z + {plane.d} = 0" + "\n" +
                $"            Yields projected point:" + "\n" +
                $"               ({projectedPoint.x}, {projectedPoint.y}, {projectedPoint.z})" + "\n"
            );

            //-----------------------------------------------------------//
            //-----------------------------------------------------------//

            return projectedPoint;
        }
        public static double ComputeAngleBetweenThreePoints(Point pointShared, Point point1, Point point2)
        {
            // Compute the two vectors
            Point vectorA = new Point(point1.x - pointShared.x, point1.y - pointShared.y, point1.z - pointShared.z);
            Point vectorB = new Point(point2.x - pointShared.x, point2.y - pointShared.y, point2.z - pointShared.z);

            // Compute the magnitudes of the two vectors
            double magA = Math.Sqrt(vectorA.x * vectorA.x + vectorA.y * vectorA.y + vectorA.z * vectorA.z);
            double magB = Math.Sqrt(vectorB.x * vectorB.x + vectorB.y * vectorB.y + vectorB.z * vectorB.z);

            // Normalize the vectors
            vectorA.x /= magA;
            vectorA.y /= magA;
            vectorA.z /= magA;

            vectorB.x /= magB;
            vectorB.y /= magB;
            vectorB.z /= magB;

            // Compute the dot product between the two normalized vectors
            double dotProduct = vectorA.x * vectorB.x + vectorA.y * vectorB.y + vectorA.z * vectorB.z;

            // Compute the angle in radians
            double angleInRadians = Math.Acos(dotProduct);

            // Convert the angle to degrees
            double angleInDegrees = angleInRadians * (180.0 / Math.PI);

            Debug.WriteLine($"Corner angle: {angleInDegrees}°" + "\n");

            return angleInDegrees;
        }
        internal static double CalculateExcessLength(double slope, double cornerLeg)
        {
            double length;
            if (slope < 90)
            {
                Trig.AAS(true, 90 - slope, out length, cornerLeg, out _);
            }
            else
            {
                mTools.AAS(slope - 90, out length, out _, cornerLeg);
            }
            return length;
        }
        public static Matrix4x4 AlignPartToAssemblyPlane(
        Vector3 partOrigin, Vector3 partYAxis,
        Vector3 assemblyA, Vector3 assemblyB, Vector3 assemblyC)
        {
            // Determine the vectors in assembly space.
            Vector3 assemblyVectorY = Vector3.Normalize(assemblyB - assemblyA);
            Vector3 assemblyVectorX = Vector3.Normalize(assemblyC - assemblyA);

            // The plane normal can be found via cross product of vectors to B and C from A.
            Vector3 planeNormal = Vector3.Normalize(Vector3.Cross(assemblyVectorY, assemblyVectorX));

            // Determine the rotation needed to align the part's Y-axis with the assembly's Y-vector.
            Vector3 partY = Vector3.Normalize(partYAxis - partOrigin);
            Quaternion rotationToAlignY = Quaternion.CreateFromAxisAngle(
                Vector3.Normalize(Vector3.Cross(partY, assemblyVectorY)),
                (float)Math.Acos(Vector3.Dot(partY, assemblyVectorY))
            );

            // Apply the rotation to find the part's new Z-axis.
            Vector3 partZ = Vector3.UnitZ;
            partZ = Vector3.Transform(partZ, rotationToAlignY);

            // Now determine the rotation needed to align the part's Z-axis with the plane normal.
            Quaternion rotationToAlignZ = Quaternion.CreateFromAxisAngle(
                Vector3.Normalize(Vector3.Cross(partZ, planeNormal)),
                (float)Math.Acos(Vector3.Dot(partZ, planeNormal))
            );

            // Combine both rotations to form the total rotation needed.
            Quaternion totalRotation = rotationToAlignZ * rotationToAlignY;

            // Convert the quaternion to a rotation matrix.
            Matrix4x4 rotationMatrix = Matrix4x4.CreateFromQuaternion(totalRotation);

            // Debug output
            Console.WriteLine("Rotation Matrix:");
            Console.WriteLine(rotationMatrix);

            return rotationMatrix;
        }




        public static Point ShiftOffCenter(Point r4, Point s4, double shift)
        {
            // Calculate the midpoint
            Point midpoint = new Point((r4.x + s4.x) / 2, (r4.y + s4.y) / 2, (r4.z + s4.z) / 2);

            // Determine the direction vector and normalize it
            Point direction = (s4 - r4).Normalize();

            // Shift the midpoint
            return midpoint + (shift/2) * direction;
        }
        public static Point ShiftOffCenter(Point midpoint, Point bottomPoint, Point topPoint, double shift)
        {
            // Determine the direction vector and normalize it
            Point direction = (topPoint - bottomPoint).Normalize();

            // Shift the midpoint
            return midpoint + shift * direction;
        }


        public static double CalculateDistance(Point point1, Point point2)
        {
            double deltaX = point2.x - point1.x;
            double deltaY = point2.y - point1.y;
            double deltaZ = point2.z - point1.z;

            double distance = Math.Sqrt(deltaX * deltaX + deltaY * deltaY + deltaZ * deltaZ);

            return distance;
        }

        public static double CalculateAngle(Point top, Point bottom, Point vert)
        {

            // Create vectors
            Point vectorTopBottom = new Point(bottom.x - top.x, bottom.y - top.y, bottom.z - top.z);
            Point vectorBottomVert = new Point(vert.x - bottom.x, vert.y - bottom.y, vert.z - bottom.z);

            // Dot product
            double dotProduct = vectorTopBottom.x * vectorBottomVert.x +
                                vectorTopBottom.y * vectorBottomVert.y +
                                vectorTopBottom.z * vectorBottomVert.z;

            // Magnitudes
            double magnitudeTopBottom = Math.Sqrt(vectorTopBottom.x * vectorTopBottom.x +
                                                  vectorTopBottom.y * vectorTopBottom.y +
                                                  vectorTopBottom.z * vectorTopBottom.z);
            double magnitudeBottomVert = Math.Sqrt(vectorBottomVert.x * vectorBottomVert.x +
                                                   vectorBottomVert.y * vectorBottomVert.y +
                                                   vectorBottomVert.z * vectorBottomVert.z);

            // Calculate the angle in radians
            double angleRadians = Math.Acos(dotProduct / (magnitudeTopBottom * magnitudeBottomVert));

            // Convert to degrees
            double angleDegrees = angleRadians * (180 / Math.PI);

            // return supplementary pair
            return 180 - angleDegrees;
        }
    }
}
