using System.Threading;
using System.Threading.Tasks;

namespace GenerationSandbox.Tests
{
    using Microsoft.EntityFrameworkCore;

    /// <summary>
    /// Test class.
    /// </summary>
    public partial class EntityFrameworkQueryableExtensions
    {
        /// <summary>
        /// Test method.
        /// </summary>
        /// <param name="dbContext">The db context.</param>
        /// <param name="cancellationToken">The cancellation token.</param>
        /// <returns>The result.</returns>
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public async Task<int> QueryableExtensionAsync(DbContext dbContext, CancellationToken cancellationToken)
        {
            var dbSet = dbContext.Set<object>();
            if (await dbSet.AnyAsync(cancellationToken))
            {
                return await dbSet.ExecuteDeleteAsync(cancellationToken);
            }

            return 0;
        }
    }
}
