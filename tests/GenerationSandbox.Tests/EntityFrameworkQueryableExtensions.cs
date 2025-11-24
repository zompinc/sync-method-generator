using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Zomp.SyncMethodGenerator.IntegrationTests
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
        /// <param name="source">The source.</param>
        /// <param name="cancellationToken">The cancellation token.</param>
        /// <returns>The result.</returns>
        [Zomp.SyncMethodGenerator.CreateSyncVersion]
        public async Task<bool> QueryableExtensionAsync(IQueryable<object> source, CancellationToken cancellationToken)
        {
            return await source.AnyAsync(cancellationToken);
        }
    }
}
